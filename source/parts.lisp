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
   (hole-connectors (remove-if-not #'(lambda (kind) (member kind '(:pin-hole :axle-hole)))
                                   (local-connectors (the spec)) :key #'connector-kind)))

  :objects
  ((studs :type 'cylinder
          :sequence (:size (length (the stud-connectors)))
          :radius 2.4 :length 1.7
          :center (the (local-point (connector-point (nth (the-child index) (the stud-connectors)))
                                    :up 0.85))
          :orientation (alignment :rear (the (face-normal-vector :top)))
          :display-controls (the display-controls))

   (holes :type (:sequence (mapcar #'(lambda (c) (if (eql (connector-kind c) :axle-hole) 'axle-hole-mark 'cylinder))
                                   (the hole-connectors)))
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
         :width 7.9 :length (* 8 (1- (the n))) :height 7.2
         :display-controls (the display-controls))

   (end-caps :type 'cylinder
         :sequence (:size (if (> (the n) 1) 2 1))
         :radius 3.6 :length 7.9
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


(define-object axle-hole-mark (base-object)
  :documentation (:description "An axle hole: a dark cross through the part, along its own :rear.")

  :input-slots (length (radius 2.5))

  :objects
  ((arms :type 'box
         :sequence (:size 2)
         :width (if (zerop (the-child index)) 4.9 1.9)
         :height (if (zerop (the-child index)) 1.9 4.9)
         :length (the length)
         :display-controls (the display-controls))))


(define-object wheel (part-mixin)
  :documentation (:description "A wheel: a black tire on a hub of the part's color, turning on an axle along its own :rear.")

  :computed-slots
  ((diameter (* 8 (first (the size))))
   (width (* 8 (second (the size)))))

  :objects
  ((tire :type 'cylinder
         :radius (- (half (the diameter)) 0.1) :length (- (the width) 0.4)
         :display-controls (list :color :black))
   (hub :type 'cylinder
        :radius (max 4.0 (* 0.3 (the diameter))) :length (- (the width) 0.1)
        :display-controls (the display-controls))))


(define-object gear (part-mixin)
  :documentation (:description "A spur gear turning on an axle along its own :rear: a disk and its teeth.")

  :computed-slots
  ((teeth (first (the size)))
   (pitch-radius (mm (pitch-radius (the teeth))))
   (thickness (- (mm (* 2 (gear-half (the teeth)))) 0.2))
   (axis (the (face-normal-vector :rear)))
   (spoke (the (face-normal-vector :top))))

  :objects
  ((disk :type 'cylinder
         :radius (- (the pitch-radius) 0.7) :length (the thickness)
         :display-controls (the display-controls))

   (hub :type 'cylinder
        :sequence (:size (if (< (the thickness) 7.8) 1 0))
        :radius 4.0 :length 7.8
        :display-controls (the display-controls))

   (cogs :type 'box
         :sequence (:size (the teeth))
         :pseudo-inputs (radial)
         :radial (rotate-vector-d (the spoke) (* (+ (the-child index) 1/2) (/ 360 (the teeth))) (the axis))
         :width 1.4 :length (the thickness) :height 2.2
         :center (translate-along-vector (the center) (the-child radial) (- (the pitch-radius) 0.1))
         :orientation (alignment :rear (the axis) :top (the-child radial))
         :display-controls (the display-controls))))


(define-object motor (part-mixin)
  :documentation (:description "A motor: a studded body two bricks high and an output hub that turns an axle.")

  :objects
  ((body :type 'box
         :width 15.8 :length 31.8 :height 19.2
         :center (the (local-point '(0 0 24)))
         :display-controls (the display-controls))
   (output :type 'cylinder
           :radius 3.6 :length 7.8
           :center (the (local-point '(0 50 24)))
           :display-controls (list :color :orange))))


(define-object controller (part-mixin)
  :documentation (:description "A controller: a body two bricks high with a screen on top, four ports at the front and pin holes down both sides.")

  :objects
  ((body :type 'box
         :width 31.8 :length 47.8 :height 19.2
         :center (the (local-point '(0 0 24)))
         :display-controls (the display-controls))
   (screen :type 'box
           :width 22.0 :length 22.0 :height 0.4
           :center (the (local-point '(0 0 48) :up 0.2))
           :display-controls (list :color :grey-slate-dark))
   (ports :type 'box
          :sequence (:size 4)
          :width 5.0 :length 0.4 :height 4.0
          :center (the (local-point (list (row-offset (the-child index) 4) -60 24) :up 0))
          :display-controls (list :color :black))))


(defun part-class (spec)
  (ecase (spec-type spec)
    ((:brick :plate :tile :technic-brick) 'brick)
    (:beam 'beam)
    (:axle 'axle)
    (:pin 'pin)
    (:wheel 'wheel)
    (:gear 'gear)
    (:motor 'motor)
    (:controller 'controller)))
