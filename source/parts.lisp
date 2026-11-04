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


(defun shaft-segments (spec)
  "List of (kind from to), in LDU along the part's own :rear, that draw a
pin, a half pin or an axle-pin: :pin and :stud round, :axle a cross,
:collar the ring between them.  The ends and collars are where the
LDraw library puts them."
  (let ((size (spec-size spec)))
    (ecase (spec-type spec)
      (:pin (if (eql (first size) 3)
                '((:pin -30 30) (:collar -11 -9))
                '((:pin -20 20) (:collar -1 1))))
      (:half-pin '((:pin -20 0) (:collar -1 1) (:stud 0 4)))
      (:axle-pin (if (eql (first size) 2)
                     '((:pin -30 10) (:collar 9 11) (:axle 10 30))
                     '((:pin -20 0) (:collar -1 1) (:axle 0 20)))))))


(define-object axle-segment (base-object)
  :documentation (:description "A stretch of axle: a cross, along its own :rear.")

  :input-slots (length (radius 2.4))

  :objects
  ((vane :type 'box
         :sequence (:size 2)
         :width (if (zerop (the-child index)) 4.8 1.8)
         :height (if (zerop (the-child index)) 1.8 4.8)
         :length (the length)
         :display-controls (the display-controls))))


(define-object pin (part-mixin)
  :documentation (:description "A Technic pin, half pin or axle-pin along its own :rear, drawn from its segments.")

  :computed-slots ((segments (shaft-segments (the spec))))

  :objects
  ((pieces :type (:sequence (mapcar #'(lambda (s) (if (eql (first s) :axle) 'axle-segment 'cylinder))
                                    (the segments)))
           :sequence (:size (length (the segments)))
           :pseudo-inputs (segment)
           :segment (nth (the-child index) (the segments))
           :radius (if (eql (first (the-child segment)) :collar) 3.0 2.4)
           :length (- (mm (- (third (the-child segment)) (second (the-child segment)))) 0.1)
           :center (the (local-point (list 0 (/ (+ (second (the-child segment)) (third (the-child segment))) 2) 0)))
           :display-controls (the display-controls))))


(define-object bush (part-mixin)
  :documentation (:description "A Technic bush: a short sleeve with two flanges, on an axle along its own :rear.")

  :objects
  ((sleeve :type 'cylinder :radius 3.0 :length 7.8 :display-controls (the display-controls))
   (flanges :type 'cylinder
            :sequence (:size 2)
            :radius 3.6 :length 0.8
            :center (the (local-point (list 0 (if (zerop (the-child index)) -9 9) 0)))
            :display-controls (the display-controls))))


(define-object cross-block (part-mixin)
  :documentation (:description "A Technic cross block, 2 or 3 holes long along its own :rear: an axle hole across it, pin holes through it.")

  :computed-slots ((n (first (the size))))

  :objects
  ((body :type 'box
         :width 7.8 :height 7.8 :length (- (* 8 (the n)) 0.2)
         :center (the (local-point (list 0 (if (= (the n) 3) 0 10) 0)))
         :display-controls (the display-controls))))


(define-object l-beam (part-mixin)
  :documentation (:description "A Technic L-beam 3 by 5: holes through along its own :rear, five up along :top from the anchor to the corner, three along :right from the corner.")

  :objects
  ((arms :type 'box
         :sequence (:size 2)
         :width (if (zerop (the-child index)) 16.0 7.2)
         :height (if (zerop (the-child index)) 7.2 32.0)
         :length 7.9
         :center (the (local-point (if (zerop (the-child index)) '(20 0 80) '(0 0 40))))
         :display-controls (the display-controls))
   (end-caps :type 'cylinder
         :sequence (:size 3)
         :radius 3.6 :length 7.9
         :center (the (local-point (nth (the-child index) '((0 0 0) (0 0 80) (40 0 80)))))
         :display-controls (the display-controls))))


(define-object round-tile (part-mixin)
  :documentation (:description "A round tile 1 by 1: a smooth disk a plate high.")

  :objects
  ((disk :type 'cylinder
         :radius 3.9 :length 3.2
         :center (the (local-point '(0 0 4)))
         :orientation (alignment :rear (the (face-normal-vector :top)))
         :display-controls (the display-controls))))


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
    ((:pin :half-pin :axle-pin) 'pin)
    (:bush 'bush)
    (:cross-block 'cross-block)
    (:l-beam 'l-beam)
    (:round-tile 'round-tile)
    (:wheel 'wheel)
    (:gear 'gear)
    (:motor 'motor)
    (:controller 'controller)))
