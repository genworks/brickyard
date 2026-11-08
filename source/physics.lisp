;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; Rigid bodies on a flat table: a pose and a velocity in the plane, made
;; of circles and rectangles, pushing one another about with friction.
;; Nothing here knows of bricks; table.lisp says what is on the table.
;;
;; Everything on a table stays on it, so the third dimension buys little
;; and the plane buys a solver small enough to read: find what touches,
;; then a few rounds of impulses at each touch (sequential impulses), and
;; move.  Lengths are millimeters, masses kilograms, times seconds.
;;

(defstruct (circle-shape (:constructor make-circle-shape (x y radius &optional (z 0d0) owner)))
  "A circle at x y in its body's own frame.  One on a moving part of the
body says how high it is and whose it is, and moves at vx vy in the world."
  (x 0d0 :type double-float) (y 0d0 :type double-float) (radius 0d0 :type double-float)
  (z 0d0 :type double-float) (vx 0d0 :type double-float) (vy 0d0 :type double-float)
  owner)

(defstruct (rect-shape (:constructor make-rect-shape (x y half-x half-y)))
  "A rectangle about x y in its body's own frame, square to that frame."
  (x 0d0 :type double-float) (y 0d0 :type double-float)
  (half-x 0d0 :type double-float) (half-y 0d0 :type double-float))

(defstruct rigid
  "A body.  An inverse mass of zero is a thing that does not move.  drag
and spin-drag are how fast the floor slows it, in mm/s/s and rad/s/s;
a body with its own way of meeting the floor (table.lisp) leaves them 0."
  name
  (x 0d0 :type double-float) (y 0d0 :type double-float) (angle 0d0 :type double-float)
  (vx 0d0 :type double-float) (vy 0d0 :type double-float) (spin 0d0 :type double-float)
  (inverse-mass 0d0 :type double-float) (inverse-inertia 0d0 :type double-float)
  (restitution 0d0 :type double-float) (grip 0.4d0 :type double-float)
  (drag 0d0 :type double-float) (spin-drag 0d0 :type double-float)
  (height 1d6 :type double-float)
  (cos 1d0 :type double-float) (sin 0d0 :type double-float)
  (reach 0d0 :type double-float)
  shapes)

(defstruct touch
  "Two bodies touching at px py, the normal nx ny from a to b."
  a b
  (px 0d0 :type double-float) (py 0d0 :type double-float)
  (nx 0d0 :type double-float) (ny 0d0 :type double-float)
  (depth 0d0 :type double-float) (bounce 0d0 :type double-float)
  (normal-mass 0d0 :type double-float) (tangent-mass 0d0 :type double-float)
  (pushed 0d0 :type double-float) (rubbed 0d0 :type double-float)
  (extra-x 0d0 :type double-float) (extra-y 0d0 :type double-float)
  shape-a shape-b)

(defparameter *solver-rounds* 8 "Integer. Rounds of impulses a step.")

(defparameter *slop* 0.2d0 "Millimeters of overlap left alone, so that resting things stay still.")

(defparameter *settling* 0.2d0 "The share of an overlap a step puts right.")

(defparameter *bounce-speed* 40d0 "Millimeters a second below which nothing bounces.")

(defun shape-reach (shape)
  "How far shape goes from its body's origin."
  (etypecase shape
    (circle-shape (+ (circle-shape-radius shape)
                     (sqrt (+ (expt (circle-shape-x shape) 2) (expt (circle-shape-y shape) 2)))))
    (rect-shape (sqrt (+ (expt (+ (abs (rect-shape-x shape)) (rect-shape-half-x shape)) 2)
                         (expt (+ (abs (rect-shape-y shape)) (rect-shape-half-y shape)) 2))))))

(defun settle-rigid! (rigid)
  "Bring what rigid keeps of its own angle and shapes up to date."
  (setf (rigid-cos rigid) (cos (rigid-angle rigid))
        (rigid-sin rigid) (sin (rigid-angle rigid))
        (rigid-reach rigid) (reduce #'max (rigid-shapes rigid) :key #'shape-reach :initial-value 0d0))
  rigid)

(declaim (inline world-x world-y))

(defun world-x (rigid x y)
  (+ (rigid-x rigid) (- (* x (rigid-cos rigid)) (* y (rigid-sin rigid)))))

(defun world-y (rigid x y)
  (+ (rigid-y rigid) (+ (* x (rigid-sin rigid)) (* y (rigid-cos rigid)))))

(defun add-touch (touches a b px py nx ny depth)
  (cons (make-touch :a a :b b :px px :py py :nx nx :ny ny :depth depth) touches))

(defun circles-touching (a sa b sb touches)
  (let* ((ax (world-x a (circle-shape-x sa) (circle-shape-y sa)))
         (ay (world-y a (circle-shape-x sa) (circle-shape-y sa)))
         (dx (- (world-x b (circle-shape-x sb) (circle-shape-y sb)) ax))
         (dy (- (world-y b (circle-shape-x sb) (circle-shape-y sb)) ay))
         (apart (sqrt (+ (* dx dx) (* dy dy))))
         (depth (- (+ (circle-shape-radius sa) (circle-shape-radius sb)) apart)))
    (if (and (plusp depth) (> apart 1d-9))
        (let ((nx (/ dx apart)) (ny (/ dy apart)))
          (add-touch touches a b
                     (+ ax (* nx (circle-shape-radius sa))) (+ ay (* ny (circle-shape-radius sa)))
                     nx ny depth))
        touches)))

(defun rect-circle-touching (a sa b sb touches)
  "Rectangle sa of a against circle sb of b; the normal runs from a to b."
  (let* ((cx (world-x b (circle-shape-x sb) (circle-shape-y sb)))
         (cy (world-y b (circle-shape-x sb) (circle-shape-y sb)))
         (radius (circle-shape-radius sb))
         ;; the circle's center in the rectangle's own frame
         (rx (- cx (rigid-x a)))
         (ry (- cy (rigid-y a)))
         (lx (- (+ (* rx (rigid-cos a)) (* ry (rigid-sin a))) (rect-shape-x sa)))
         (ly (- (- (* ry (rigid-cos a)) (* rx (rigid-sin a))) (rect-shape-y sa)))
         (hx (rect-shape-half-x sa))
         (hy (rect-shape-half-y sa))
         (qx (max (- hx) (min hx lx)))
         (qy (max (- hy) (min hy ly)))
         (inside? (and (= qx lx) (= qy ly))))
    (flet ((touch (nx ny depth)
             (let ((wx (- (* nx (rigid-cos a)) (* ny (rigid-sin a))))
                   (wy (+ (* nx (rigid-sin a)) (* ny (rigid-cos a)))))
               (add-touch touches a b (- cx (* wx radius)) (- cy (* wy radius)) wx wy depth))))
      (if inside?
          ;; the center is inside: out by the nearest side
          (let ((to-x (- hx (abs lx))) (to-y (- hy (abs ly))))
            (if (< to-x to-y)
                (touch (if (minusp lx) -1d0 1d0) 0d0 (+ to-x radius))
                (touch 0d0 (if (minusp ly) -1d0 1d0) (+ to-y radius))))
          (let* ((dx (- lx qx)) (dy (- ly qy))
                 (apart (sqrt (+ (* dx dx) (* dy dy)))))
            (if (< apart radius)
                (touch (/ dx apart) (/ dy apart) (- radius apart))
                touches))))))

(defun rect-corners (rigid shape)
  "List of the four corners of shape in the world, as (x . y)."
  (let ((x (rect-shape-x shape)) (y (rect-shape-y shape))
        (hx (rect-shape-half-x shape)) (hy (rect-shape-half-y shape)))
    (loop for (sx sy) in '((-1 -1) (1 -1) (1 1) (-1 1))
          collect (cons (world-x rigid (+ x (* sx hx)) (+ y (* sy hy)))
                        (world-y rigid (+ x (* sx hx)) (+ y (* sy hy)))))))

(defun point-in-rect-p (rigid shape px py)
  (let* ((rx (- px (rigid-x rigid)))
         (ry (- py (rigid-y rigid)))
         (lx (- (+ (* rx (rigid-cos rigid)) (* ry (rigid-sin rigid))) (rect-shape-x shape)))
         (ly (- (- (* ry (rigid-cos rigid)) (* rx (rigid-sin rigid))) (rect-shape-y shape))))
    (and (<= (abs lx) (+ (rect-shape-half-x shape) 1d-6))
         (<= (abs ly) (+ (rect-shape-half-y shape) 1d-6)))))

(defun rects-touching (a sa b sb touches)
  "Separating axes: the axis of least overlap gives the normal, and the
corners of each rectangle inside the other the points of contact."
  (let* ((ax (world-x a (rect-shape-x sa) (rect-shape-y sa)))
         (ay (world-y a (rect-shape-x sa) (rect-shape-y sa)))
         (bx (world-x b (rect-shape-x sb) (rect-shape-y sb)))
         (by (world-y b (rect-shape-x sb) (rect-shape-y sb)))
         (dx (- bx ax)) (dy (- by ay))
         (ac (rigid-cos a)) (as (rigid-sin a)) (bc (rigid-cos b)) (bs (rigid-sin b))
         (ahx (rect-shape-half-x sa)) (ahy (rect-shape-half-y sa))
         (bhx (rect-shape-half-x sb)) (bhy (rect-shape-half-y sb))
         (least most-positive-double-float)
         (nx 0d0) (ny 0d0))
    (declare (double-float least nx ny))
    (flet ((overlap (ux uy)
             (let* ((ra (+ (* ahx (abs (+ (* ac ux) (* as uy))))
                           (* ahy (abs (- (* ac uy) (* as ux))))))
                    (rb (+ (* bhx (abs (+ (* bc ux) (* bs uy))))
                           (* bhy (abs (- (* bc uy) (* bs ux))))))
                    (along (+ (* dx ux) (* dy uy)))
                    (depth (- (+ ra rb) (abs along))))
               (when (< depth least)
                 (setf least depth
                       nx (if (minusp along) (- ux) ux)
                       ny (if (minusp along) (- uy) uy)))
               (plusp depth))))
      (if (and (overlap ac as) (overlap (- as) ac) (overlap bc bs) (overlap (- bs) bc))
          (let ((points (append (remove-if-not #'(lambda (p) (point-in-rect-p b sb (car p) (cdr p)))
                                               (rect-corners a sa))
                                (remove-if-not #'(lambda (p) (point-in-rect-p a sa (car p) (cdr p)))
                                               (rect-corners b sb)))))
            ;; rectangles crossed like a plus sign keep no corner inside
            (dolist (p (or points (list (cons (/ (+ ax bx) 2) (/ (+ ay by) 2)))) touches)
              (setf touches (add-touch touches a b (car p) (cdr p) nx ny least))))
          touches))))

(defun over-p (shape other)
  "True when shape, on a moving part, passes above the body other."
  (and (circle-shape-p shape) (circle-shape-owner shape)
       (> (- (circle-shape-z shape) (circle-shape-radius shape)) (rigid-height other))))

(defun shapes-touching (a sa b sb touches)
  (when (or (over-p sa b) (over-p sb a))
    (return-from shapes-touching touches))
  (let ((found (etypecase sa
                 (circle-shape (etypecase sb
                                 (circle-shape (circles-touching a sa b sb touches))
                                 (rect-shape (rect-circle-touching b sb a sa touches))))
                 (rect-shape (etypecase sb
                               (circle-shape (rect-circle-touching a sa b sb touches))
                               (rect-shape (rects-touching a sa b sb touches)))))))
    ;; what is new knows its shapes, and how fast they move on their bodies
    (loop for rest on found
          until (eq rest touches)
          do (let* ((touch (first rest))
                    (flipped? (not (eq (touch-a touch) a)))
                    (first-shape (if flipped? sb sa))
                    (second-shape (if flipped? sa sb)))
               (setf (touch-shape-a touch) first-shape
                     (touch-shape-b touch) second-shape)
               (when (circle-shape-p first-shape)
                 (decf (touch-extra-x touch) (circle-shape-vx first-shape))
                 (decf (touch-extra-y touch) (circle-shape-vy first-shape)))
               (when (circle-shape-p second-shape)
                 (incf (touch-extra-x touch) (circle-shape-vx second-shape))
                 (incf (touch-extra-y touch) (circle-shape-vy second-shape)))))
    found))

(defun in-reach-p (a b)
  (let ((dx (- (rigid-x a) (rigid-x b)))
        (dy (- (rigid-y a) (rigid-y b)))
        (reach (+ (rigid-reach a) (rigid-reach b))))
    (< (+ (* dx dx) (* dy dy)) (* reach reach))))

(defun touches-of (rigids)
  "List of the touches among rigids."
  (let (touches)
    (loop for (a . others) on rigids
          do (dolist (b others)
               (when (and (plusp (+ (rigid-inverse-mass a) (rigid-inverse-mass b)
                                    (rigid-inverse-inertia a) (rigid-inverse-inertia b)))
                          (in-reach-p a b))
                 (dolist (sa (rigid-shapes a))
                   (dolist (sb (rigid-shapes b))
                     (setf touches (shapes-touching a sa b sb touches)))))))
    touches))

(defun point-velocity (rigid px py)
  "Values: the velocity of the point of rigid now at px py."
  (values (- (rigid-vx rigid) (* (rigid-spin rigid) (- py (rigid-y rigid))))
          (+ (rigid-vy rigid) (* (rigid-spin rigid) (- px (rigid-x rigid))))))

(defun push-rigid! (rigid px py jx jy)
  "An impulse jx jy on rigid at the point px py."
  (incf (rigid-vx rigid) (* jx (rigid-inverse-mass rigid)))
  (incf (rigid-vy rigid) (* jy (rigid-inverse-mass rigid)))
  (incf (rigid-spin rigid) (* (rigid-inverse-inertia rigid)
                              (- (* (- px (rigid-x rigid)) jy) (* (- py (rigid-y rigid)) jx)))))

(defun mass-along (rigid px py ux uy)
  "The inverse mass rigid shows to a push along ux uy at px py."
  (let ((lever (- (* (- px (rigid-x rigid)) uy) (* (- py (rigid-y rigid)) ux))))
    (+ (rigid-inverse-mass rigid) (* (rigid-inverse-inertia rigid) lever lever))))

(defun relative-velocity (touch)
  (let ((a (touch-a touch)) (b (touch-b touch)) (px (touch-px touch)) (py (touch-py touch)))
    (multiple-value-bind (ax ay) (point-velocity a px py)
      (multiple-value-bind (bx by) (point-velocity b px py)
        (values (+ (- bx ax) (touch-extra-x touch)) (+ (- by ay) (touch-extra-y touch)))))))

(defun prepare-touch! (touch)
  (let* ((a (touch-a touch)) (b (touch-b touch))
         (px (touch-px touch)) (py (touch-py touch))
         (nx (touch-nx touch)) (ny (touch-ny touch))
         (tx (- ny)) (ty nx))
    (setf (touch-normal-mass touch) (/ (+ (mass-along a px py nx ny) (mass-along b px py nx ny)))
          (touch-tangent-mass touch) (/ (+ (mass-along a px py tx ty) (mass-along b px py tx ty))))
    (multiple-value-bind (vx vy) (relative-velocity touch)
      (let ((closing (+ (* vx nx) (* vy ny))))
        (when (< closing (- *bounce-speed*))
          (setf (touch-bounce touch)
                (* (- closing) (max (rigid-restitution a) (rigid-restitution b)))))))))

(defun press-touch! (touch dt)
  "One round of impulses at touch: apart along the normal, friction across it."
  (let* ((a (touch-a touch)) (b (touch-b touch))
         (px (touch-px touch)) (py (touch-py touch))
         (nx (touch-nx touch)) (ny (touch-ny touch))
         (tx (- ny)) (ty nx)
         (settle (* (/ *settling* dt) (max 0d0 (- (touch-depth touch) *slop*)))))
    (flet ((impulse (j ux uy)
             (push-rigid! a px py (- (* j ux)) (- (* j uy)))
             (push-rigid! b px py (* j ux) (* j uy))))
      (multiple-value-bind (vx vy) (relative-velocity touch)
        (let* ((wanted (* (touch-normal-mass touch)
                          (+ (- (+ (* vx nx) (* vy ny))) settle (touch-bounce touch))))
               (total (max 0d0 (+ (touch-pushed touch) wanted)))
               (j (- total (touch-pushed touch))))
          (setf (touch-pushed touch) total)
          (impulse j nx ny)))
      (multiple-value-bind (vx vy) (relative-velocity touch)
        (let* ((limit (* (min (rigid-grip a) (rigid-grip b)) (touch-pushed touch)))
               (wanted (* (touch-tangent-mass touch) (- (+ (* vx tx) (* vy ty)))))
               (total (max (- limit) (min limit (+ (touch-rubbed touch) wanted))))
               (j (- total (touch-rubbed touch))))
          (setf (touch-rubbed touch) total)
          (impulse j tx ty))))))

(defun slow-on-floor! (rigid dt)
  "Take off what the floor does to rigid in dt."
  (let ((speed (sqrt (+ (expt (rigid-vx rigid) 2) (expt (rigid-vy rigid) 2))))
        (lost (* (rigid-drag rigid) dt)))
    (when (plusp lost)
      (if (<= speed lost)
          (setf (rigid-vx rigid) 0d0 (rigid-vy rigid) 0d0)
          (let ((keep (/ (- speed lost) speed)))
            (setf (rigid-vx rigid) (* keep (rigid-vx rigid))
                  (rigid-vy rigid) (* keep (rigid-vy rigid)))))))
  (let ((lost (* (rigid-spin-drag rigid) dt)))
    (when (plusp lost)
      (setf (rigid-spin rigid)
            (if (<= (abs (rigid-spin rigid)) lost)
                0d0
                (- (rigid-spin rigid) (* lost (signum (rigid-spin rigid)))))))))

(defun step-rigids! (rigids dt)
  "Move rigids on by dt: the floor, then what touches, then the moves.
Answers the touches, each with what it pressed and rubbed."
  (dolist (rigid rigids) (slow-on-floor! rigid dt))
  (let ((touches (touches-of rigids)))
    (mapc #'prepare-touch! touches)
    (dotimes (round *solver-rounds*)
      (dolist (touch touches) (press-touch! touch dt)))
    (dolist (rigid rigids)
      (when (plusp (rigid-inverse-mass rigid))
        (incf (rigid-x rigid) (* dt (rigid-vx rigid)))
        (incf (rigid-y rigid) (* dt (rigid-vy rigid))))
      ;; a body that cannot move but can turn is hinged at its origin
      (when (plusp (rigid-inverse-inertia rigid))
        (incf (rigid-angle rigid) (* dt (rigid-spin rigid)))
        (setf (rigid-cos rigid) (cos (rigid-angle rigid))
              (rigid-sin rigid) (sin (rigid-angle rigid)))))
    touches))
