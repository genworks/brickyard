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


(define-object assembly (base-object)
  :documentation (:description "A build: the parts its specs name, and what holds them together.")

  :input-slots
  (("List of part specs (see specs.lisp). Settable, so the editor can add, move and remove parts."
    part-specs *sample-build* :settable))

  :computed-slots
  ((connections (connections (the part-specs)))
   (collisions (collisions (the part-specs)))
   (groups (groups (length (the part-specs)) (the connections)))
   ("List of indices. Parts not held, through any chain of joints, to the first part."
    loose (sort (apply #'append (rest (the groups))) #'<))
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
          :orientation (alignment :top (the (face-normal-vector (spec-up (the-child place))))
                                  :rear (the (face-normal-vector (spec-rear (the-child place)))))
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
