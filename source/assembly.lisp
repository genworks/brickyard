;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

(defparameter *sample-build*
  '((:type :technic-brick :size (8) :at (-30 0 0))
    (:type :technic-brick :size (8) :at (30 0 0))
    (:type :plate :size (4 8) :at (0 0 24))
    (:type :brick :size (2 4) :at (0 -40 32))
    (:type :axle :size (10) :at (0 -60 14) :rear :right)
    (:type :pin :at (40 60 14) :rear :right)
    (:type :pin :at (40 20 14) :rear :right)
    (:type :beam :size (5) :at (50 20 14)))
  "List of specs. A small chassis: two Technic bricks tied by a plate,
an axle through both, and a beam pinned to one side.")

(defparameter *robot-build*
  '((:type :motor :at (-60 0 0) :rear :left)
    (:type :motor :at (60 0 0) :rear :right)
    (:type :plate :size (10 6) :color :blue :at (0 0 48))
    (:type :controller :at (0 0 56))
    (:type :axle :size (4) :at (-140 0 24) :rear :right)
    (:type :axle :size (4) :at (140 0 24) :rear :right)
    (:type :wheel :size (5 2) :at (-150 0 24) :rear :right)
    (:type :wheel :size (5 2) :at (150 0 24) :rear :right)
    (:type :brick :size (2 2 9) :color :black :at (0 40 -24)))
  "List of specs. A drive base: two motors under a plate, each turning a
wheel on an axle, the controller on top, and a skid at the front.")

(defparameter *gearbox-build*
  '((:type :technic-brick :size (8) :at (-30 0 0))
    (:type :technic-brick :size (8) :at (30 0 0))
    (:type :motor :at (100 -20 -10) :rear :left)
    (:type :axle :size (5) :at (0 -20 14) :rear :right)
    (:type :axle :size (8) :at (0 20 14) :rear :right)
    (:type :gear :size (8) :at (0 -20 14) :rear :right)
    (:type :gear :size (24) :color :tan :at (0 20 14) :rear :right)
    (:type :wheel :size (5 2) :at (-70 20 14) :rear :right))
  "List of specs. A reduction: the motor turns an 8-tooth gear that turns
a 24-tooth gear two studs away, which carries a wheel round at a third
of the motor's speed, the other way.")


(define-object assembly (base-object)
  :documentation (:description "A build: the parts its specs name, and what holds them together.")

  :input-slots
  (("List of part specs (see specs.lisp). Settable, so the editor can add, move and remove parts."
    part-specs *sample-build* :settable))

  :computed-slots
  (("The build surveyed once: each part's box and connectors, and the joints of touching pairs."
    survey (survey (the part-specs)))
   (connections (connections (the part-specs) (the survey)))
   (collisions (collisions (the part-specs) (the survey)))
   (groups (groups (length (the part-specs)) (the connections)))
   ("List of indices. Parts not held, through any chain of joints, to the
main body: the largest group, or of the largest the one with the earliest
part."
    loose (let ((main (reduce #'(lambda (best g) (if (> (length g) (length best)) g best))
                              (the groups) :initial-value nil)))
            (sort (apply #'append (remove main (the groups) :test #'eq)) #'<)))
   (power-flow (multiple-value-list (power-flow (the part-specs) (the survey))))
   ("List of (motor wheel speed): each wheel a motor turns, at what speed against it."
    drives (first (the power-flow)))
   ("List of indices. Parts a motor would turn at two speeds at once: a jammed train."
    jammed (second (the power-flow)))
   ("List of indices. The parts the build stands on, loose parts left out."
    ground-contacts (let ((held (remove-if #'(lambda (i) (member i (the loose)))
                                           (iota (length (the part-specs))))))
                      (mapcar #'(lambda (k) (nth k held))
                              (ground-contacts (mapcar #'(lambda (i) (nth i (the part-specs))) held)))))
   ("List of (count type size color), most numerous first."
    bill-of-materials
    (let (counts)
      (dolist (spec (the part-specs))
        (let* ((key (list (spec-type spec) (spec-size spec) (spec-color spec)))
               (entry (assoc key counts :test #'equal)))
          (if entry (incf (rest entry)) (push (cons key 1) counts))))
      (mapcar #'(lambda (entry) (cons (rest entry) (first entry)))
              (stable-sort (nreverse counts) #'> :key #'rest)))))

  :objects
  ((parts :type (:sequence (mapcar #'part-class (the part-specs)))
          :sequence (:size (length (the part-specs)))
          :pseudo-inputs (place)
          :place (nth (the-child index) (the part-specs))
          :spec (the-child place)
          :center (destructuring-bind (x y z) (spec-at (the-child place))
                    (translate (the center) :right (mm x) :rear (mm y) :up (mm z)))
          ;; the part's own :top and :rear in the world, its rotation's
          ;; third and second columns -- square or at an angle alike
          :orientation (let* ((rows (spec-rotation (the-child place)))
                              (top (mapcar #'float (column rows 2)))
                              (rear (mapcar #'float (column rows 1))))
                         (alignment :top (make-vector (first top) (second top) (third top))
                                    :rear (make-vector (first rear) (second rear) (third rear))))
          :display-controls (list :color (spec-color (the-child place)))))

  :functions
  ((add-part!
    (spec)
    (the (set-slot! :part-specs (append (the part-specs) (list spec))))
    (1- (length (the part-specs))))

   (remove-part!
    (index)
    (the (set-slot! :part-specs (append (subseq (the part-specs) 0 index)
                                        (nthcdr (1+ index) (the part-specs))))))

   (replace-part!
    (index spec)
    (let ((specs (copy-list (the part-specs))))
      (setf (nth index specs) spec)
      (the (set-slot! :part-specs specs))))

   (snap-part!
    (index at &key (radius *snap-radius*))
    "Move part index to at and pull it onto the nearest clear joint with
the others.  Returns the new spec, or nil (and the part stays put) when
nothing within radius takes it."
    (let* ((specs (the part-specs))
           (moved (snap-spec (move-spec (nth index specs) at)
                             (append (subseq specs 0 index) (nthcdr (1+ index) specs))
                             :radius radius)))
      (when moved (the (replace-part! index moved)))
      moved))))
