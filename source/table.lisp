;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; A robot on a table: a build drives itself about a walled mat, by a
;; program, and pushes what it meets.  The table is the size robot
;; competitions use, a mat of 2362 by 1143 mm inside low walls; what is
;; printed on the mat and what stands on it are this file's own.
;;
;; A robot here runs a program it was given and nobody steers it, as at a
;; competition; pilot.lisp has the programs and the run, pybricks.lisp
;; writes a program out for a real hub.
;;
;; The build is one rigid body.  Its mass and its center come from its
;; parts; what it weighs on each wheel and skid from where they stand;
;; and a motor pushes through each wheel it turns as far as the wheel's
;; grip allows.  It does not tip or climb.  A motor that turns no wheel on
;; the mat turns an arm, an arm being what its shaft carries by an axle
;; hole: (:arm 60) swings it, against its weight and whatever it meets, and
;; it holds there.  An arm stands for itself in the plane as a set of
;; circles, each with a height, so one raised clear of the pieces passes
;; over them and one lowered sweeps them along; it pushes, and does not
;; lift what it meets.
;;

(defparameter *table*
  '(:width 2362 :depth 1143 :wall 40 :wall-height 70
    ;; x y and heading in degrees, 0 along the table, 90 across it
    :start (190 571 0)
    ;; x y width depth color: what is printed, from the mat's corner
    :print ((0 311 380 520 (0.72 0.85 0.72))
            (380 561 1340 20 (0.15 0.15 0.15))
            (1200 332 20 229 (0.15 0.15 0.15))
            (1200 581 20 229 (0.15 0.15 0.15)))
    ;; name x y radius color
    :rings ((:red 1900 571 180 (0.85 0.25 0.2))
            (:blue 1210 960 150 (0.25 0.4 0.8))
            (:yellow 1210 182 150 (0.92 0.75 0.2)))
    ;; kind x y
    :pieces ((:ball 760 571) (:ball 1000 820) (:ball 1000 320) (:ball 1520 571)
             (:ball 1720 950) (:crate 1460 300) (:crate 1520 850))
    ;; x y width depth, about its center: stays where it stands
    :fixed ((1660 230 40 240)))
  "Plist. The table: its size in millimeters, what is printed on the mat, and what stands on it.")

(defparameter *pieces*
  '((:ball :radius 24 :grams 30 :restitution 0.5d0 :drag 300d0 :color (0.95 0.55 0.1))
    (:crate :half 32 :grams 60 :restitution 0.1d0 :drag 3900d0 :spin-drag 60d0 :color (0.6 0.42 0.25)))
  "List of (kind properties): what can be pushed about.  Drag is what the floor takes off, mm/s/s.")

(defparameter *part-grams* '((:motor . 60) (:controller . 180) (:color-sensor . 15)
                              (:distance-sensor . 20) (:force-sensor . 15)
                              ;; the hub 63 g by LEGO's sheet, with an estimated 120 g battery;
                              ;; the motors as listed for sale; the sensors and wheel estimated
                              (:spike-hub . 183) (:spike-large-motor . 76) (:spike-medium-motor . 54)
                              (:spike-color-sensor . 15) (:spike-distance-sensor . 25)
                              (:spike-force-sensor . 15) (:spike-wheel . 25))
  "List of (type . grams). A typical part of that type, for the parts that are not plastic.")

(defparameter *hollow* 0.45d0
  "The share of a part's box that is plastic: a brick is an open shell.")

(defparameter *plastic* 1.05d0 "Grams to the cubic centimeter.")

(defparameter *free-speed* (* 1000 (/ pi 180))
  "Radians a second: a motor at full power with nothing to turn, Pybricks'
rated top speed for the SPIKE angular motors, 1000 deg/s.")

(defparameter *stall-torque* 180000d0 "Kilogram square millimeters per square second: 0.18 N m, a motor held still.")

(defparameter *motor-figures*
  '((:motor 1000 0.18)
    ;; LEGO Education's SPIKE Prime technical specifications, at 7.2 V:
    ;; no load 175 and 185 rpm, stall 25 and 18 N cm
    (:spike-large-motor 1050 0.25)
    (:spike-medium-motor 1110 0.18))
  "List of (type deg/s N-m): each motor's speed with nothing to turn and its torque held still.")

(defun motor-free-speed (type)
  (* (or (second (assoc type *motor-figures*)) 1000) (/ pi 180)))

(defun motor-stall (type)
  "The motor's stall torque in kilogram square millimeters per square second."
  (* 1d6 (or (third (assoc type *motor-figures*)) 0.18d0)))

(defparameter *tire-grip* 0.8d0 "A tire's coefficient of friction on the mat.")

(defparameter *skid-grip* 0.25d0 "The same for a part that slides.")

(defparameter *rolling* 0.02d0 "The share of its load a free wheel loses to rolling.")

(defparameter *gravity* 9810d0 "Millimeters a second a second.")

(defparameter *reach-height* 64
  "Millimeters. A part of the robot's body higher off the mat than this passes over what is on it.")

(defparameter *arm-reach* 90
  "Millimeters. An arm higher than this meets nothing; below it, what is tall enough.")

(defparameter *tick* 0.005d0 "Seconds. The step of a run.")

(defparameter *frame-ticks* 7 "Integer. Ticks to a frame the page plays back: about 30 a second.")

(defparameter *match-seconds* 150 "Integer. How long a run may last.")

(defparameter *program-limit* 200 "Integer. The most steps in a program.")

(defstruct drive
  "A motor on the robot: id its part's, side -1 left, 1 right, 0 in the
middle; rolled the millimeters its wheels have counted, and last what
they had counted a tick ago; angle how far it has turned, and parts the
(id speed point axis) of what it turns.  top is how fast its wheels' rims
go at full power, mm/s.  For a real hub: ratio how fast its first wheel or
arm turns against it, and clockwise whether Pybricks' Direction.CLOCKWISE
makes positive drive forward, or swing an arm the positive way."
  id (side 0) (power 0d0 :type double-float) (rolled 0d0 :type double-float)
  (last 0d0 :type double-float) (angle 0d0 :type double-float) (wheels 0) parts
  (top 1d0 :type double-float) (ratio 1) clockwise
  (free-speed *free-speed* :type double-float) (stall *stall-torque* :type double-float))

(defstruct tread
  "Where the robot meets the mat, in the body's own frame.  With a radius
it rolls toward fx fy; with a drive as well a motor turns it, ratio
times the motor's speed; with no radius it slides."
  (x 0d0 :type double-float) (y 0d0 :type double-float)
  (fx 0d0 :type double-float) (fy 0d0 :type double-float)
  radius drive (ratio 1d0 :type double-float)
  (grip 0d0 :type double-float) (load 0d0 :type double-float)
  ;; the rim's speed when the wheel slips, nil while it grips
  (spin nil))

(defstruct arm
  "What a motor swings on the robot.  point is its pivot, x y from the
center of mass and z above the mat; axis the line it turns about; ratio
its speed against the motor's; angle, rate and target in radians.  balls
are (x y z shape), each from the pivot as built.  center is its center
of mass from the pivot, and load the torque what it met put on it."
  drive point axis (ratio 1d0 :type double-float)
  (angle 0d0 :type double-float) (rate 0d0 :type double-float)
  (target 0d0 :type double-float) (speed 0.5d0 :type double-float)
  (kilograms 0d0 :type double-float) (inertia 0d0 :type double-float)
  center balls (load 0d0 :type double-float) (blocked nil))

(defstruct sensor
  "A sensor on the robot: kind its part type, id its part's; x y z where
it senses from, x y from the center of mass and z above the mat, and dx
dy dz the way it looks, all in the robot's own frame, millimeters."
  kind id x y z dx dy dz)

(defstruct robot
  "A build ready for the table.  forward is the way it faces in its own
frame; center x y and lift, in millimeters, say how the page places it.
fixed is the shapes that do not move on it.  track and wheel-diameter are
a drive base's, millimeters; track is nil without a wheel motor each side."
  rigid treads drives arms fixed center lift forward track wheel-diameter sensors hub-up)

(defparameter *sensor-faces*
  '((:color-sensor (0 0 0) (0 0 -1))
    (:distance-sensor (0 10 12) (0 1 0))
    (:force-sensor (0 36 12) (0 1 0))
    (:spike-color-sensor (0 -49 0) (0 -1 0))
    (:spike-distance-sensor (0 -69 0) (0 -1 0))
    (:spike-force-sensor (0 -70 0) (0 -1 0)))
  "List of (type point direction): where each sensor senses from and the way it looks, in its own LDU.")

(defparameter *sensor-kinds*
  '((:spike-color-sensor . :color-sensor) (:spike-distance-sensor . :distance-sensor)
    (:spike-force-sensor . :force-sensor))
  "Alist: what kind of sensor each SPIKE sensor is.")

(defun sensor-of (spec center lowest)
  (destructuring-bind (point dir) (rest (assoc (spec-type spec) *sensor-faces*))
    (let ((rows (spec-rotation spec)))
      (destructuring-bind (x y z) (mapcar #'mm (v+ (spec-at spec) (rotate rows point)))
        (destructuring-bind (dx dy dz) (mapcar #'(lambda (v) (float v 1d0)) (rotate rows dir))
          (make-sensor :kind (or (cdr (assoc (spec-type spec) *sensor-kinds*)) (spec-type spec)) :id (spec-id spec)
                       :x (float (- x (first center)) 1d0) :y (float (- y (second center)) 1d0)
                       :z (float (- z (mm lowest)) 1d0) :dx dx :dy dy :dz dz))))))

(defun mat-color (table x y)
  "The color printed on the mat at x y: the rings over the print over the mat itself."
  (or (loop for (nil cx cy radius color) in (getf table :rings)
            for r = (sqrt (+ (expt (- x cx) 2) (expt (- y cy) 2)))
            when (<= (* 0.8d0 radius) r radius) return color)
      (loop for (px py w d color) in (reverse (getf table :print))
            when (and (<= px x (+ px w)) (<= py y (+ py d))) return color)
      '(0.94 0.94 0.9)))

(defun color-name (rgb)
  "What a color sensor calls rgb, of the colors Pybricks' color() reports
unless told otherwise: red, yellow, green, blue, white, or none.  Black
is none: too dark to be a color."
  (destructuring-bind (r g b) rgb
    (let* ((top (max r g b)) (low (min r g b))
           (saturation (if (plusp top) (/ (- top low) top) 0))
           (hue (cond ((= top low) 0)
                      ((= top r) (mod (* 60 (/ (- g b) (- top low))) 360))
                      ((= top g) (+ 120 (* 60 (/ (- b r) (- top low)))))
                      (t (+ 240 (* 60 (/ (- r g) (- top low))))))))
      (cond ((< top 0.25) :none)
            ((< saturation 0.3) (if (> top 0.6) :white :none))
            ((or (< hue 25) (>= hue 330)) :red)
            ((< hue 75) :yellow)
            ((< hue 170) :green)
            ((< hue 260) :blue)
            (t :none)))))

(defun part-grams (spec)
  (or (cdr (assoc (spec-type spec) *part-grams*))
      (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (world-extent spec)
        (* *hollow* *plastic* 1/1000 (mm (- x1 x0)) (mm (- y1 y0)) (mm (- z1 z0))))))

(defun part-box (spec)
  "List of center x, center y, width and depth, in millimeters."
  (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (world-extent spec)
    (declare (ignore z0 z1))
    (list (mm (/ (+ x0 x1) 2)) (mm (/ (+ y0 y1) 2)) (mm (- x1 x0)) (mm (- y1 y0)))))

(defun mass-properties (specs)
  "Three values: kilograms, the center (x y) in millimeters, and the
turning inertia about it, each part taken as an even box."
  (let* ((grams (mapcar #'part-grams specs))
         (boxes (mapcar #'part-box specs))
         (total (reduce #'+ grams))
         (cx (/ (reduce #'+ (mapcar #'(lambda (g b) (* g (first b))) grams boxes)) total))
         (cy (/ (reduce #'+ (mapcar #'(lambda (g b) (* g (second b))) grams boxes)) total))
         (inertia (reduce #'+ (mapcar #'(lambda (g b)
                                          (destructuring-bind (x y w d) b
                                            (* g (+ (/ (+ (* w w) (* d d)) 12)
                                                    (expt (- x cx) 2) (expt (- y cy) 2)))))
                                      grams boxes))))
    (values (/ total 1000d0) (list (float cx 1d0) (float cy 1d0)) (/ inertia 1000d0))))

(defun support-loads (points weight)
  "List of what each of points, (x y) from the center of mass, carries of
weight, as on even springs; or nil when the build would tip."
  (let* ((n (length points))
         (mx (/ (reduce #'+ points :key #'first) n))
         (my (/ (reduce #'+ points :key #'second) n))
         (xx 0d0) (yy 0d0) (xy 0d0))
    (dolist (p points)
      (let ((x (- (first p) mx)) (y (- (second p) my)))
        (incf xx (* x x)) (incf yy (* y y)) (incf xy (* x y))))
    (let ((det (- (* xx yy) (* xy xy))))
      (when (> det 1d-3)
        ;; an even share each, plus the tilt that brings the moments to nothing
        (let* ((a (* weight (- mx)))
               (b (* weight (- my)))
               (tilt-x (/ (- (* yy a) (* xy b)) det))
               (tilt-y (/ (- (* xx b) (* xy a)) det))
               (loads (mapcar #'(lambda (p)
                                  (+ (/ weight n)
                                     (* tilt-x (- (first p) mx))
                                     (* tilt-y (- (second p) my))))
                              points)))
          (when (every #'(lambda (l) (>= l 0d0)) loads) loads))))))

(defun roller-line (spec)
  "List of point and axis of a part that rolls on the mat, or nil."
  (let ((connector (case (spec-type spec)
                     (:tyre (find :tyre-seat (world-connectors spec) :key #'connector-kind))
                     ((:wheel :rim :spike-wheel) (find :axle-hole (world-connectors spec) :key #'connector-kind)))))
    (when (and connector (< (abs (third (connector-dir connector))) 1/100))
      (list (connector-point connector) (shared-axis (connector-dir connector))))))

(defun tread-of (spec center drive ratio)
  "The tread of ground part spec, center the build's center of mass."
  (let ((line (roller-line spec)))
    (if line
        (destructuring-bind ((x y z) (ax ay az)) line
          (declare (ignore z az))
          (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (world-extent spec)
            (declare (ignore x0 y0 x1 y1))
            (make-tread :x (- (mm x) (first center)) :y (- (mm y) (second center))
                        :fx (float (- ay) 1d0) :fy (float ax 1d0)
                        :radius (mm (/ (- z1 z0) 2)) :drive drive :ratio (float ratio 1d0)
                        :grip *tire-grip*)))
        (destructuring-bind (x y w d) (part-box spec)
          (declare (ignore w d))
          (make-tread :x (- x (first center)) :y (- y (second center)) :grip *skid-grip*)))))

(defun drives-of (specs survey)
  "Three values: a list of drives; a hash table from each part a motor
turns to (drive . ratio); and a list of (drive shaft . parts), the arms.
A motor's forward is the way its first wheel rolls forward, as a program
would set a motor's polarity."
  (multiple-value-bind (all jammed trains) (motors-turning specs survey)
    (declare (ignore all jammed))
    (let ((turned (make-hash-table)) drives arms)
      (dolist (train trains)
        (destructuring-bind (m speeds) train
          (let* ((entries (turning-entries specs survey speeds))
                 (rides (riding-groups specs survey speeds))
                 (first-wheel (find-if #'(lambda (e) (roller-line (nth (first e) specs))) entries))
                 (polarity (if (and first-wheel (minusp (second first-wheel))) -1 1))
                 (drive (make-drive :id (spec-id (nth m specs))
                                    :free-speed (float (motor-free-speed (spec-type (nth m specs))) 1d0)
                                    :stall (float (motor-stall (spec-type (nth m specs))) 1d0)))
                 ;; Pybricks sees a motor turn clockwise looking at its
                 ;; shaft: about the shaft's way out of the motor, backwards
                 (shaft (first (first (motor-seeds specs survey m))))
                 (along (and shaft (second (turning-line (nth shaft specs)))))
                 (out (connector-dir (find :axle-hole (world-connectors (nth m specs)) :key #'connector-kind)))
                 (facing (if along (dot along out) 1)))
            (cond (first-wheel
                   (setf (drive-ratio drive) (abs (second first-wheel))
                         (drive-clockwise drive) (plusp (* polarity facing))))
                  (rides
                   (let ((ratio (gethash (first (first rides)) speeds)))
                     (setf (drive-ratio drive) (abs ratio)
                           (drive-clockwise drive) (minusp (* ratio facing))))))
            (when (or first-wheel rides)
              (setf (drive-parts drive)
                    (mapcar #'(lambda (e)
                                (destructuring-bind (index speed point axis) e
                                  (setf (gethash index turned) (cons drive (* polarity speed)))
                                  (list (spec-id (nth index specs)) (* polarity speed) point axis)))
                            entries))
              (push drive drives)
              (dolist (ride rides)
                (push (cons drive ride) arms))))))
      (values (nreverse drives) turned (nreverse arms)))))

(defun part-balls (spec owner)
  "List of (x y z shape): balls that fill spec's box, in millimeters as
built, for a part that moves.  Thin parts take a few larger balls."
  (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (world-extent spec)
    (let* ((low (list (mm x0) (mm y0) (mm z0)))
           (sizes (list (mm (- x1 x0)) (mm (- y1 y0)) (mm (- z1 z0))))
           (thin (reduce #'min sizes))
           (radius (max 4d0 (/ thin 2) (/ (sqrt (/ (* (reduce #'* sizes) (/ 1 thin)) 48)) 1.6d0)))
           (counts (mapcar #'(lambda (size) (max 1 (ceiling size (* 1.5d0 radius)))) sizes))
           balls)
      (flet ((place (i axis)
               (+ (nth axis low) (* (nth axis sizes) (/ (+ i 1/2) (nth axis counts))))))
        (dotimes (i (first counts))
          (dotimes (j (second counts))
            (dotimes (k (third counts))
              (push (list (place i 0) (place j 1) (place k 2)
                          (make-circle-shape 0d0 0d0 (float radius 1d0) 0d0 owner))
                    balls))))
        balls))))

(defun arm-of (drive shaft parts specs center lowest)
  "The arm on drive that parts make, swinging about shaft."
  (destructuring-bind (point axis) (turning-line (nth shaft specs))
    (let* ((members (mapcar #'(lambda (i) (nth i specs)) parts))
           (pivot (mapcar #'mm point))
           (unit (mapcar #'(lambda (x) (float x 1d0)) axis))
           (arm (make-arm :drive drive
                          :point (list (- (first pivot) (first center)) (- (second pivot) (second center))
                                       (- (third pivot) (mm lowest)))
                          :axis unit))
           (grams (mapcar #'part-grams members))
           (total (reduce #'+ grams))
           (middles (mapcar #'(lambda (s)
                                (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (world-extent s)
                                  (v- (list (mm (/ (+ x0 x1) 2)) (mm (/ (+ y0 y1) 2)) (mm (/ (+ z0 z1) 2)))
                                      pivot)))
                            members))
           (ratio (second (find (spec-id (nth shaft specs)) (drive-parts drive) :key #'first))))
      (setf (arm-ratio arm) (float ratio 1d0)
            (arm-kilograms arm) (/ total 1000d0)
            (arm-center arm) (mapcar #'(lambda (k) (/ (reduce #'+ (mapcar #'(lambda (g m) (* g (nth k m))) grams middles)) total))
                                     '(0 1 2))
            (arm-inertia arm) (/ (reduce #'+ (mapcar #'(lambda (g m)
                                                         (let ((across (cross m unit)))
                                                           (* g (+ 25 (dot across across)))))
                                                     grams middles))
                                 1000d0)
            (arm-balls arm) (mapcan #'(lambda (s)
                                        (mapcar #'(lambda (ball)
                                                    (destructuring-bind (x y z shape) ball
                                                      (destructuring-bind (dx dy dz) (v- (list x y z) pivot)
                                                        (list dx dy dz shape))))
                                                (part-balls s arm)))
                                    members))
      arm)))

(defun robot-of (assembly)
  "The robot assembly makes, or an error saying why it cannot drive."
  (let* ((specs (the-object assembly part-specs))
         (survey (the-object assembly survey))
         (loose (the-object assembly loose)))
    (cond ((null specs) (error "There is nothing to put on the table."))
          (loose (error "~a of its parts are held to nothing." (length loose)))
          ((the-object assembly jammed) (error "Its train is jammed.")))
    (multiple-value-bind (kilograms center inertia) (mass-properties specs)
      (multiple-value-bind (drives turned rides) (drives-of specs survey)
        (let* ((swinging (mapcan #'(lambda (ride) (copy-list (cddr ride))) rides))
               (staying (remove-if #'(lambda (i) (member i swinging)) (iota (length specs))))
               (stays (mapcar #'(lambda (i) (nth i specs)) staying))
               ;; what it stands on: an arm that rests on the mat will lift
               (ground (mapcar #'(lambda (k) (nth k staying)) (ground-contacts stays)))
               (treads (mapcar #'(lambda (i)
                                   (let ((turn (gethash i turned)))
                                     (tread-of (nth i specs) center (car turn) (or (cdr turn) 1))))
                               ground))
               (driven (remove-if-not #'tread-drive treads))
               (weight (* kilograms *gravity*))
               (loads (support-loads (mapcar #'(lambda (tr) (list (tread-x tr) (tread-y tr))) treads) weight)))
          (unless driven (error "No motor turns a wheel that stands on the mat."))
          (unless loads (error "It would tip over: it needs to stand on at least three points around its middle."))
          (mapc #'(lambda (tread load) (setf (tread-load tread) load)) treads loads)
          (let* ((fx (/ (reduce #'+ driven :key #'tread-fx) (length driven)))
                 (fy (/ (reduce #'+ driven :key #'tread-fy) (length driven)))
                 (lowest (reduce #'min stays :key #'(lambda (s) (third (first (world-extent s))))))
                 (arms (mapcar #'(lambda (ride)
                                   (destructuring-bind (drive shaft . parts) ride
                                     (arm-of drive shaft parts specs center lowest)))
                               rides))
                 (shapes (loop for spec in stays
                               for bottom = (mm (- (third (first (world-extent spec))) lowest))
                               when (< bottom *reach-height*)
                                 collect (destructuring-bind (x y w d) (part-box spec)
                                           (make-rect-shape (float (- x (first center)) 1d0)
                                                            (float (- y (second center)) 1d0)
                                                            (float (/ w 2) 1d0) (float (/ d 2) 1d0))))))
            (dolist (tread driven)
              (incf (drive-wheels (tread-drive tread)))
              (setf (drive-top (tread-drive tread)) (* (drive-free-speed (tread-drive tread)) (tread-ratio tread) (tread-radius tread))))
            (dolist (drive drives)
              ;; which side of the way it faces its wheels stand on
              (let ((across (loop for tread in driven
                                  when (eq (tread-drive tread) drive)
                                    sum (- (* (tread-x tread) fy) (* (tread-y tread) fx)))))
                (setf (drive-side drive) (cond ((> across 5) 1) ((< across -5) -1) (t 0)))))
            (let ((robot (make-robot :rigid (make-rigid :name :robot :inverse-mass (/ kilograms)
                                                        :inverse-inertia (/ inertia)
                                                        :restitution 0.1d0 :shapes shapes)
                                     :treads treads :drives drives :arms arms :fixed shapes
                                     :center center :forward (list fx fy)
                                     :lift (- (mm lowest))
                                     :wheel-diameter (* 2 (tread-radius (first driven)))
                                     :hub-up (let ((hub (find :spike-hub stays :key #'spec-type)))
                                               (and hub (rotate (spec-rotation hub) '(0 0 1))))
                                     :sensors (loop for spec in stays
                                                    when (assoc (spec-type spec) *sensor-faces*)
                                                      collect (sensor-of spec center lowest)))))
              ;; the drive base's track: from the left wheels' middle to the right's
              (flet ((across (side)
                       (let ((treads (remove side driven :key #'(lambda (tr) (drive-side (tread-drive tr)))
                                                         :test-not #'eql)))
                         (and treads (/ (reduce #'+ treads :key #'(lambda (tr) (- (* (tread-x tr) fy) (* (tread-y tr) fx))))
                                        (length treads))))))
                (let ((left (across -1)) (right (across 1)))
                  (setf (robot-track robot) (and left right (- right left)))))
              (settle-rigid! (robot-rigid robot))
              (incf (rigid-reach (robot-rigid robot)) (reduce #'max arms :key #'arm-span :initial-value 0d0))
              (pose-arms! robot)
              robot)))))))

(defun arm-span (arm)
  "How far the arm can reach from the robot's center of mass."
  (+ (sqrt (+ (expt (first (arm-point arm)) 2) (expt (second (arm-point arm)) 2)))
     (reduce #'max (arm-balls arm)
             :key #'(lambda (ball)
                      (destructuring-bind (x y z shape) ball
                        (+ (circle-shape-radius shape) (sqrt (+ (* x x) (* y y) (* z z))))))
             :initial-value 0d0)))

(defun pose-arm! (arm rigid)
  "Put arm's balls where its angle says, and say how fast each goes.
Answers the height of its lowest point and the balls low enough to meet
what is on the mat."
  (let ((rows (axis-rotation (arm-axis arm) (arm-angle arm)))
        (lowest most-positive-double-float)
        active)
    (destructuring-bind (px py pz) (arm-point arm)
      (destructuring-bind (ax ay az) (arm-axis arm)
        (dolist (ball (arm-balls arm))
          (destructuring-bind (x y z shape) ball
            (destructuring-bind (rx ry rz) (rotate rows (list x y z))
              (let* ((height (+ pz rz))
                     (bottom (- height (circle-shape-radius shape)))
                     (rate (arm-rate arm))
                     (vx (* rate (- (* ay rz) (* az ry))))
                     (vy (* rate (- (* az rx) (* ax rz)))))
                (setf lowest (min lowest bottom)
                      (circle-shape-x shape) (float (+ px rx) 1d0)
                      (circle-shape-y shape) (float (+ py ry) 1d0)
                      (circle-shape-z shape) (float height 1d0)
                      (circle-shape-vx shape) (float (- (* vx (rigid-cos rigid)) (* vy (rigid-sin rigid))) 1d0)
                      (circle-shape-vy shape) (float (+ (* vx (rigid-sin rigid)) (* vy (rigid-cos rigid))) 1d0))
                (when (< bottom *arm-reach*)
                  (push shape active))))))))
    (values lowest active)))

(defun pose-arms! (robot)
  "Bring the robot's shapes up to date with its arms."
  (let ((rigid (robot-rigid robot)) (shapes (robot-fixed robot)))
    (dolist (arm (robot-arms robot))
      (multiple-value-bind (lowest active) (pose-arm! arm rigid)
        (declare (ignore lowest))
        (setf shapes (append active shapes))))
    (setf (rigid-shapes rigid) shapes)))

(defun arm-weight-torque (arm)
  "What its weight does about its axis, where it now is."
  (destructuring-bind (x y z) (rotate (axis-rotation (arm-axis arm) (arm-angle arm)) (arm-center arm))
    (declare (ignore z))
    (let ((weight (* (arm-kilograms arm) *gravity*)))
      (destructuring-bind (ax ay az) (arm-axis arm)
        (declare (ignore az))
        ;; the lever crossed with straight down, along the axis
        (* weight (- (* ax (- y)) (* ay (- x))))))))

(defun swing-arm! (arm robot dt)
  "The motor drives arm toward its target and holds it there, against
its weight and what it met.  Solved with the speed the torque itself will
give, as for a wheel."
  (let* ((drive (arm-drive arm))
         (ratio (arm-ratio arm))
         (power (within (* 4d0 (- (arm-target arm) (arm-angle arm))) (arm-speed arm)))
         (stiffness (/ (drive-stall drive) (* ratio ratio (drive-free-speed drive))))
         (free (* power (drive-free-speed drive) ratio))
         (rate (/ (+ (* (arm-inertia arm) (arm-rate arm))
                     (* dt (+ (* stiffness free) (arm-weight-torque arm) (arm-load arm))))
                  (+ (arm-inertia arm) (* dt stiffness))))
         (before (arm-angle arm)))
    (setf (drive-power drive) power
          (arm-rate arm) rate
          (arm-angle arm) (+ before (* dt rate))
          (arm-load arm) 0d0
          (arm-blocked arm) nil)
    ;; the mat stops it, going down
    (let ((low (pose-arm! arm (robot-rigid robot))))
      (when (< low -1d0)
        (setf (arm-angle arm) before)
        (let ((back (pose-arm! arm (robot-rigid robot))))
          (if (< back low)
              ;; it was lower where it stood: rising is free
              (setf (arm-angle arm) (+ before (* dt rate)))
              (setf (arm-rate arm) 0d0 (arm-blocked arm) t)))))
    (setf (drive-angle drive) (/ (arm-angle arm) ratio))))

(defun feel-touches! (robot touches dt)
  "What the touches of this step did to the arms: a torque for the next."
  (when (robot-arms robot)
    (let ((rigid (robot-rigid robot)))
      (dolist (touch touches)
        (let* ((sa (touch-shape-a touch))
               (sb (touch-shape-b touch))
               (first? (and (circle-shape-p sa) (circle-shape-owner sa)))
               (shape (if first? sa sb))
               (arm (and (circle-shape-p shape) (circle-shape-owner shape))))
          (when arm
            ;; the impulse on the arm's side of the touch
            (let* ((sign (if first? -1d0 1d0))
                   (nx (touch-nx touch)) (ny (touch-ny touch))
                   (jx (* sign (- (* (touch-pushed touch) nx) (* (touch-rubbed touch) ny))))
                   (jy (* sign (+ (* (touch-pushed touch) ny) (* (touch-rubbed touch) nx))))
                   (c (rigid-cos rigid)) (s (rigid-sin rigid)))
              (destructuring-bind (px py pz) (arm-point arm)
                (destructuring-bind (ax ay az) (arm-axis arm)
                  ;; in the robot's own frame
                  (let* ((bx (+ (* jx c) (* jy s)))
                         (by (- (* jy c) (* jx s)))
                         (rx (- (circle-shape-x shape) px))
                         (ry (- (circle-shape-y shape) py))
                         (rz (- (circle-shape-z shape) pz)))
                    ;; (r x j) along the axis; j has no height
                    (incf (arm-load arm)
                          (/ (+ (* ax (- (* rz by)))
                                (* ay (* rz bx))
                                (* az (- (* rx by) (* ry bx))))
                             dt))))))))))))

(defun count-treads! (robot dt)
  "After a step, how far each driven wheel turned: as the robot went, where
it gripped, as its motor spun it, where it slipped."
  (let ((rigid (robot-rigid robot)))
    (dolist (tread (robot-treads robot))
      (let ((drive (tread-drive tread)))
        (when (and drive (tread-radius tread))
          (let* ((px (world-x rigid (tread-x tread) (tread-y tread)))
                 (py (world-y rigid (tread-x tread) (tread-y tread)))
                 (fx (- (* (tread-fx tread) (rigid-cos rigid)) (* (tread-fy tread) (rigid-sin rigid))))
                 (fy (+ (* (tread-fx tread) (rigid-sin rigid)) (* (tread-fy tread) (rigid-cos rigid))))
                 (share (drive-wheels drive))
                 (surface (or (tread-spin tread)
                              (multiple-value-bind (vx vy) (point-velocity rigid px py)
                                (+ (* vx fx) (* vy fy))))))
            (incf (drive-rolled drive) (/ (* surface dt) share))
            (incf (drive-angle drive) (/ (* surface dt) (* share (tread-radius tread) (tread-ratio tread))))))))))

(defun roll-tread! (rigid tread dt)
  "What tread does between rigid and the mat in dt."
  (let* ((px (world-x rigid (tread-x tread) (tread-y tread)))
         (py (world-y rigid (tread-x tread) (tread-y tread)))
         (limit (* (tread-grip tread) (tread-load tread)))
         (fx (- (* (tread-fx tread) (rigid-cos rigid)) (* (tread-fy tread) (rigid-sin rigid))))
         (fy (+ (* (tread-fx tread) (rigid-sin rigid)) (* (tread-fy tread) (rigid-cos rigid))))
         (radius (tread-radius tread))
         (drive (tread-drive tread)))
    (multiple-value-bind (vx vy) (point-velocity rigid px py)
      (flet ((resist (ux uy along limit)
               ;; friction: up to limit, and no more than stops it
               (let* ((stop (/ (abs along) (mass-along rigid px py ux uy)))
                      (j (* (- (signum along)) (min (* limit dt) (* 0.5d0 stop)))))
                 (push-rigid! rigid px py (* j ux) (* j uy)))))
        (if (null radius)
            ;; a skid: friction against the way it is going
            (let ((speed (sqrt (+ (* vx vx) (* vy vy)))))
              (when (> speed 1d-6)
                (resist (/ vx speed) (/ vy speed) speed limit)))
            (let ((rolling (+ (* vx fx) (* vy fy)))
                  (sideways (- (* vy fx) (* vx fy))))
              (resist (- fy) fx sideways limit)
              (if drive
                  ;; the motor's line of torque against speed, solved with
                  ;; the speed the push itself will give: stiff otherwise
                  (let* ((ratio (tread-ratio tread))
                         (share (drive-wheels drive))
                         (reach (* (abs ratio) radius))
                         (free (* (drive-power drive) (drive-free-speed drive) ratio radius))
                         (stiffness (/ (drive-stall drive) (* (drive-free-speed drive) reach reach share)))
                         (give (* dt (mass-along rigid px py fx fy)))
                         (force (/ (* stiffness (- free rolling)) (+ 1d0 (* stiffness give))))
                         (grips? (<= (abs force) limit))
                         (held (if grips? force (* limit (signum force))))
                         ;; a slipping wheel spins as its motor lets it
                         (surface (if grips?
                                      (+ rolling (* held give))
                                      (- free (/ held stiffness)))))
                    (push-rigid! rigid px py (* held dt fx) (* held dt fy))
                    ;; a gripping wheel turns as the robot goes, which the
                    ;; step will tell (count-treads!); a slipping one spins
                    (setf (tread-spin tread) (if grips? nil surface)))
                  (resist fx fy rolling (* *rolling* (tread-load tread))))))))))

;;
;; Helpers
;;

(defun angle-difference (a b)
  "a less b, the short way round, in radians."
  (let ((d (mod (- a b) (* 2 pi))))
    (if (> d pi) (- d (* 2 pi)) d)))

(defun within (x limit) (max (- limit) (min limit x)))

;;
;; The table set up, and a run on it
;;

(defun piece-rigid (kind x y index)
  (let ((properties (rest (assoc kind *pieces*))))
    (destructuring-bind (&key radius half grams restitution drag spin-drag color) properties
      (declare (ignore color))
      (let ((mass (/ grams 1000d0)))
        (settle-rigid!
         (make-rigid :name (list kind index) :x (float x 1d0) :y (float y 1d0)
                     :inverse-mass (/ mass)
                     :inverse-inertia (/ (if radius
                                             (* 0.4d0 mass radius radius)
                                             (* mass (/ (* 2 half half) 3))))
                     :restitution restitution :drag drag :spin-drag (float (or spin-drag 0d0) 1d0)
                     :height (float (* 2 (or radius half)) 1d0)
                     :shapes (list (if radius
                                       (make-circle-shape 0d0 0d0 (float radius 1d0))
                                       (make-rect-shape 0d0 0d0 (float half 1d0) (float half 1d0))))))))))

(defun fixed-rigid (x y width depth)
  (settle-rigid!
   (make-rigid :name :fixed :x (float x 1d0) :y (float y 1d0) :restitution 0.3d0
               :shapes (list (make-rect-shape 0d0 0d0 (/ width 2d0) (/ depth 2d0))))))

(defun table-walls (table)
  (destructuring-bind (&key width depth wall &allow-other-keys) table
    (let ((half (/ wall 2)))
      (list (fixed-rigid (/ width 2) (- half) (+ width (* 2 wall)) wall)
            (fixed-rigid (/ width 2) (+ depth half) (+ width (* 2 wall)) wall)
            (fixed-rigid (- half) (/ depth 2) wall depth)
            (fixed-rigid (+ width half) (/ depth 2) wall depth)))))

(defun table-pieces (table)
  (loop for (kind x y) in (getf table :pieces)
        for index from 0
        collect (piece-rigid kind x y index)))

(defun place-robot! (robot table)
  "Stand robot on the table's start, facing as it says.  Answers the heading it holds."
  (destructuring-bind (x y heading) (getf table :start)
    (let* ((rigid (robot-rigid robot))
           (forward (robot-forward robot))
           (angle (- (* heading (/ pi 180)) (atan (second forward) (first forward)))))
      (setf (rigid-x rigid) (float x 1d0)
            (rigid-y rigid) (float y 1d0)
            (rigid-angle rigid) (float angle 1d0)
            (rigid-cos rigid) (cos angle)
            (rigid-sin rigid) (sin angle))
      angle)))

(defun ring-of (rigid table)
  "The name of the ring rigid's center is in, or nil."
  (loop for (name x y radius) in (getf table :rings)
        when (< (sqrt (+ (expt (- (rigid-x rigid) x) 2) (expt (- (rigid-y rigid) y) 2))) radius)
          return name))

(defun tenths (x) (round (* 10 x)))

(defun frame-of (seconds robot pieces)
  "A frame for the page, in whole numbers: time, the robot's center and
angle, each drive's angle, each piece's center and angle."
  (let ((rigid (robot-rigid robot)))
    (coerce (append (list (round (* 1000 seconds)) (tenths (rigid-x rigid)) (tenths (rigid-y rigid))
                          (round (* 1000 (rigid-angle rigid))))
                    (mapcar #'(lambda (d) (round (* 1000 (drive-angle d)))) (robot-drives robot))
                    (mapcan #'(lambda (p) (list (tenths (rigid-x p)) (tenths (rigid-y p))
                                                (round (* 1000 (rigid-angle p)))))
                            pieces))
            'vector)))

(defun still-p (rigids)
  (every #'(lambda (r) (and (< (+ (abs (rigid-vx r)) (abs (rigid-vy r))) 1d0)
                            (< (abs (rigid-spin r)) 0.01d0)))
         rigids))

;;
;; The table as the page draws it
;;

(defun x3d-color (color) (format nil "~{~,3f~^ ~}" color))

(defun x3d-box (x y z width depth height color &optional name)
  (format nil "<Transform ~@[DEF=\"~a\" ~]translation=\"~,2f ~,2f ~,2f\"><Shape><Appearance><Material diffuseColor=\"~a\"/></Appearance><Box size=\"~,2f ~,2f ~,2f\"/></Shape></Transform>"
          name x y z (x3d-color color) width depth height))

(defun x3d-ring (x y z outer inner color)
  "A flat ring: all one layer, for printed layers close together shimmer."
  (let ((sides 48))
    (format nil "<Transform translation=\"~,2f ~,2f ~,2f\"><Shape><Appearance><Material diffuseColor=\"~a\"/></Appearance><IndexedFaceSet solid=\"false\" coordIndex=\"~{~a ~a ~a ~a -1~^ ~}\"><Coordinate point=\"~{~,2f ~,2f 0~^ ~}\"/></IndexedFaceSet></Shape></Transform>"
            x y z (x3d-color color)
            (loop for i below sides
                  for next = (mod (1+ i) sides)
                  append (list (* 2 i) (* 2 next) (1+ (* 2 next)) (1+ (* 2 i))))
            (loop for i below sides
                  for angle = (* 2 pi (/ i sides))
                  append (list (* outer (cos angle)) (* outer (sin angle))
                               (* inner (cos angle)) (* inner (sin angle)))))))

(defun x3d-ball (radius color name)
  (format nil "<Transform DEF=\"~a\"><Transform translation=\"0 0 ~,2f\"><Shape><Appearance><Material diffuseColor=\"~a\"/></Appearance><Sphere radius=\"~,2f\"/></Shape></Transform></Transform>"
          name radius (x3d-color color) radius))

(defun table-x3d (&optional (table *table*))
  "The table as one X3D document: walls, mat, and each piece a Transform tN."
  (destructuring-bind (&key width depth wall wall-height print rings pieces fixed &allow-other-keys) table
    (with-output-to-string (s)
      (format s "<X3D profile=\"Interchange\" version=\"4.0\"><Scene><Transform DEF=\"table\">")
      (write-string (x3d-box (/ width 2) (/ depth 2) -6 (+ width (* 2 wall)) (+ depth (* 2 wall)) 12 '(0.94 0.94 0.9)) s)
      (let ((long (+ width (* 2 wall))) (color '(0.25 0.2 0.15)) (z (/ wall-height 2)))
        (write-string (x3d-box (/ width 2) (- (/ wall 2)) z long wall wall-height color) s)
        (write-string (x3d-box (/ width 2) (+ depth (/ wall 2)) z long wall wall-height color) s)
        (write-string (x3d-box (- (/ wall 2)) (/ depth 2) z wall depth wall-height color) s)
        (write-string (x3d-box (+ width (/ wall 2)) (/ depth 2) z wall depth wall-height color) s))
      (loop for (x y w d color) in print
            do (write-string (x3d-box (+ x (/ w 2)) (+ y (/ d 2)) 0.5 w d 1 color) s))
      (loop for (nil x y radius color) in rings
            do (write-string (x3d-ring x y 1 radius (* 0.8 radius) color) s))
      (loop for (x y w d) in fixed
            do (write-string (x3d-box x y 40 w d 80 '(0.4 0.42 0.45)) s))
      (loop for (kind nil nil) in pieces
            for index from 0
            do (destructuring-bind (&key radius half color &allow-other-keys) (rest (assoc kind *pieces*))
                 (let ((name (format nil "t~a" index)))
                   (write-string
                    (if radius
                        (x3d-ball radius color name)
                        (format nil "<Transform DEF=\"~a\">~a</Transform>" name
                                (x3d-box 0 0 half (* 2 half) (* 2 half) (* 2 half) color)))
                    s))))
      (format s "</Transform></Scene></X3D>"))))
