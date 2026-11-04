;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The geometry layer.  A part's center is its spec's anchor and its
;; orientation the spec's :up and :rear, both given by the assembly;
;; studs and holes are drawn from the spec's own connectors.  A hole is
;; a dark disk through the part, a hair wider than a pin: open-source
;; Gendl has no booleans to cut one.
;;

(define-object part-mixin (base-object)

  :input-slots (spec)

  :computed-slots
  ((part-type (spec-type (the spec)))
   (size (spec-size (the spec)))
   (stud-connectors (remove :stud (local-connectors (the spec)) :key #'connector-kind :test-not #'eql))
   (hole-connectors (remove :pin-hole (local-connectors (the spec)) :key #'connector-kind :test-not #'eql)))

  :objects
  ((studs :type 'cylinder
          :sequence (:size (length (the stud-connectors)))
          :radius 2.4 :length 1.7
          :center (the (local-point (connector-point (nth (the-child index) (the stud-connectors)))
                                    :up 0.85))
          :orientation (alignment :rear (the (face-normal-vector :top)))
          :display-controls (the display-controls))

   (holes :type 'cylinder
          :sequence (:size (length (the hole-connectors)))
          :pseudo-inputs (connector)
          :connector (nth (the-child index) (the hole-connectors))
          :radius 2.5
          :length (+ (mm (* 2 (connector-half (the-child connector)))) 0.1)
          :center (the (local-point (connector-point (the-child connector))))
          :orientation (alignment :rear (the (face-normal-vector (axis-key (connector-dir (the-child connector))))))
          :display-controls (list :color :grey-slate-dark)))

  :functions
  ((local-point
    (ldu-point &key (up 0))
    (destructuring-bind (x y z) ldu-point
      (translate (the center) :right (mm x) :rear (mm y) :up (+ (mm z) up))))))


(define-object brick (part-mixin)
  :documentation (:description "A brick, plate, tile or Technic brick: a box with studs on top.")

  :computed-slots
  ((w (first (the size))) (l (second (the size))) (h (third (the size))))

  :objects
  ((body :type 'box
         :width (- (* 8 (the w)) 0.2)
         :length (- (* 8 (the l)) 0.2)
         :height (* 3.2 (the h))
         :center (the (local-point (list 0 0 (* 4 (the h)))))
         :display-controls (the display-controls))))


(define-object beam (part-mixin)
  :documentation (:description "A studless Technic beam: a row of holes with rounded ends.")

  :computed-slots ((n (first (the size))))

  :objects
  ((body :type 'box
         :sequence (:size (if (> (the n) 1) 1 0))
         :width 7.2 :length (* 8 (1- (the n))) :height 7.2
         :display-controls (the display-controls))

   (end-caps :type 'cylinder
         :sequence (:size (if (> (the n) 1) 2 1))
         :radius 3.6 :length 7.2
         :center (the (local-point (list 0 (row-offset (* (the-child index) (1- (the n))) (the n)) 0)))
         :orientation (alignment :rear (the (face-normal-vector :right)))
         :display-controls (the display-controls))))


(define-object axle (part-mixin)
  :documentation (:description "A Technic axle: a cross section, n studs long, along its own :rear.")

  :computed-slots ((axle-length (- (* 8 (first (the size))) 0.2)))

  :objects
  ((vane :type 'box
         :sequence (:size 2)
         :width (if (zerop (the-child index)) 4.8 1.8)
         :height (if (zerop (the-child index)) 1.8 4.8)
         :length (the axle-length)
         :display-controls (the display-controls))))


(define-object pin (part-mixin)
  :documentation (:description "A Technic pin, two studs long, along its own :rear, with its collar in the middle.")

  :objects
  ((shaft :type 'cylinder :radius 2.4 :length 15.8 :display-controls (the display-controls))
   (collar :type 'cylinder :radius 3.0 :length 0.8 :display-controls (the display-controls))))


(defun part-class (spec)
  (ecase (spec-type spec)
    ((:brick :plate :tile :technic-brick) 'brick)
    (:beam 'beam)
    (:axle 'axle)
    (:pin 'pin)))
