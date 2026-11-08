;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; Programs: what a robot on the table is told.  The steps are Pybricks'
;; own, with its units and its signs, so that the one program drives the
;; robot here and, written out by pybricks.lisp, a real hub:
;;
;;   (straight 500)           drive_base.straight(500)       mm; negative goes back
;;   (turn 90)                drive_base.turn(90)            degrees; positive is clockwise
;;   (arc 300 :angle 90)      drive_base.arc(300, angle=90)  radius mm, positive to the right
;;   (arc -300 :distance 400) drive_base.arc(-300, distance=400)
;;   (drive 200 -30)          drive_base.drive(200, -30)     mm/s, deg/s, and on to the next step
;;   (drive 200 -30 1500)     the same, wait(1500), drive_base.stop()
;;   (stop)                   drive_base.stop()
;;   (settings :straight-speed 200 :turn-rate 90)   drive_base.settings(...)
;;   (arm 45 :speed 200)      arm.run_angle(200, 45)         degrees at the arm, deg/s
;;   (wait 1000)              wait(1000)                     milliseconds
;;   (wait-until (< (reflection) 30))   while not (...): wait(10)
;;   (follow-line :until (> (driven) 800))   a line follower on the reflection:
;;                            drive :speed (100) turning (reflection - :target (50)) * :gain (1)
;;                            deg/s, so a positive gain keeps the line on the sensor's right
;;   (repeat 4 (straight 300) (turn 90))     for i in range(4): ...
;;
;; A condition reads the sensors and compares: (reflection), (color),
;; (distance), (force), (pressed), (heading), (driven), each sensor by
;; its part id when there are two, (reflection 12); (< a b) (> a b)
;; (<= a b) (>= a b) (= a b) (/= a b), with colors written red, yellow,
;; green, blue, white, black and none; and (and ...) (or ...) (not ...).
;;
;; The simulated robot drives as a Pybricks drive base does: each move
;; follows a trapezoid of speed, the wheels' angles count the distance
;; and the hub's gyroscope the heading, and the settings start at
;; Pybricks' own defaults for the motors it has: 40% of the top speed
;; straight, a third of the top turn rate, three quarters of the motor's
;; acceleration.  Nothing about the table is known to the robot but what
;; its sensors read, so a wheel that slips or a wall in the way leaves it
;; where its program does not think it is.
;;

(defparameter *sample-program* "(straight 1560)
(wait 1000)
(straight -300)"
  "String. What the page's program box starts with: two balls along the line, into the red ring.")

(defparameter *motor-acceleration* (* 2000 (/ pi 180))
  "Radians a second a second: what a motor's speed control allows.  An
estimate; Pybricks' own figure is in its servo settings.")

(defparameter *step-names* '(:straight :turn :arc :drive :stop :settings :arm :wait :wait-until
                             :follow-line :repeat)
  "List of the step names a program may use.")

(defparameter *readings*
  '((:reflection . :color-sensor) (:color . :color-sensor) (:distance . :distance-sensor)
    (:force . :force-sensor) (:pressed . :force-sensor) (:heading) (:driven))
  "List of (reading . sensor type): what a condition can read, and from what.")

(defparameter *color-names* '(:red :yellow :green :blue :white :black :none)
  "List of the colors a condition can name, as Pybricks' Color does.")

(defparameter *step-limit* 5000 "Integer. The most steps a run may take, repeats counted out.")

(defstruct pilot
  "How the robot means to drive: the heading it holds, counterclockwise
radians in the table's frame, and Pybricks' four drive settings in
Pybricks' units, mm/s, mm/s/s, deg/s and deg/s/s.  motion is what
drives it between steps after a (drive ...), speed and rate what it is
told; table is the table it reads; start-rolled and start-angle what its
distance and heading count from; forces each force sensor's id and
newtons, from the last tick."
  (heading 0d0 :type double-float)
  straight-speed straight-acceleration turn-rate turn-acceleration
  motion (speed 0d0) (rate 0d0) table (start-rolled 0d0) (start-angle 0d0) forces)

;;
;; Reading and checking a program
;;

(defun check-number (value low high what)
  (unless (and (realp value) (<= low value high))
    (error "~a must be a number from ~a to ~a, not ~s." what low high value))
  (float value 1d0))

(defun checked-options (step allowed)
  "The plist after a step's place arguments, checked against allowed."
  (let ((keys (member-if #'keywordp (rest step))))
    (unless (and (evenp (length keys))
                 (loop for (key) on keys by #'cddr always (member key allowed)))
      (error "The options of ~(~s~) are ~{~(~s~)~^ ~}." (first step) allowed))
    keys))

(defun checked-condition (condition)
  "Condition, as a step may use it, or an error saying what is wrong with it."
  (flet ((operand (x)
           (cond ((realp x) (float x 1d0))
                 ((member x *color-names*) x)
                 ((and (consp x) (assoc (first x) *readings*)) (checked-condition x))
                 (t (error "~s is not a number, a color or a reading." x)))))
    (unless (consp condition)
      (error "A condition is a list like (< (reflection) 30), not ~s." condition))
    (let ((name (first condition)) (arguments (rest condition)))
      (cond ((member name '(:and :or))
             (unless arguments (error "(~(~a~)) needs a condition." name))
             (cons name (mapcar #'checked-condition arguments)))
            ((eql name :not)
             (unless (= (length arguments) 1) (error "(not ...) takes one condition."))
             (list :not (checked-condition (first arguments))))
            ((member name '(:< :> :<= :>= := :/=))
             (unless (= (length arguments) 2) (error "(~(~a~) ...) compares two things." name))
             (list name (operand (first arguments)) (operand (second arguments))))
            ((assoc name *readings*)
             (let ((sensor (cdr (assoc name *readings*))))
               (unless (<= (length arguments) (if sensor 1 0))
                 (error "(~(~a~)) takes ~:[nothing~;at most a sensor's part id~]." name sensor))
               (unless (or (null arguments) (integerp (first arguments)))
                 (error "A sensor is named by its part id, not ~s." (first arguments)))
               condition))
            (t (error "~(~s~) is not a condition: try (< (reflection) 30)." name))))))

(defun count-steps (program)
  (loop for step in program
        sum (if (eql (first step) :repeat) (1+ (count-steps (cddr step))) 1)))

(defun checked-step (step)
  "Step in its full form, or an error saying what is wrong with it."
  (unless (and (consp step) (keywordp (first step)))
    (error "A step is a list like (straight 300), not ~s." step))
  (let* ((name (first step))
         (places (loop for x in (rest step) until (keywordp x) collect x)))
    (flet ((places (n)
             (unless (= (length places) n)
               (error "~(~s~) takes ~r number~:p before its options." name n n))))
      (case name
        (:straight (places 1)
         (checked-options step nil)
         (list :straight (check-number (first places) -3000 3000 "A distance")))
        (:turn (places 1)
         (checked-options step nil)
         (list :turn (check-number (first places) -3600 3600 "A turn")))
        (:arc (places 1)
         (let ((keys (checked-options step '(:angle :distance)))
               (radius (check-number (first places) -3000 3000 "A radius")))
           (when (zerop radius) (error "An arc's radius cannot be 0: turn in place with turn."))
           (unless (= (length keys) 2)
             (error "An arc takes :angle or :distance, one of them."))
           (if (eql (first keys) :angle)
               (list :arc radius :angle (check-number (second keys) -3600 3600 "An arc's angle"))
               (list :arc radius :distance (check-number (second keys) -3000 3000 "An arc's distance")))))
        (:drive
         (unless (<= 2 (length places) 3) (error "(drive speed turn-rate) or (drive speed turn-rate milliseconds)."))
         (checked-options step nil)
         (append (list :drive (check-number (first places) -1000 1000 "A speed")
                       (check-number (second places) -1000 1000 "A turn rate"))
                 (when (third places) (list (check-number (third places) 0 150000 "Milliseconds")))))
        (:stop (places 0)
         (checked-options step nil)
         (list :stop))
        (:settings (places 0)
         (let ((keys (checked-options step '(:straight-speed :straight-acceleration :turn-rate :turn-acceleration))))
           (cons :settings
                 (loop for (key value) on keys by #'cddr
                       append (list key (check-number value 1 10000 (format nil "~(~s~)" key)))))))
        (:arm (places 1)
         (let ((keys (checked-options step '(:speed :motor))))
           (list :arm (check-number (first places) -3600 3600 "An arm's angle")
                 :speed (check-number (getf keys :speed 200) 10 1000 ":speed")
                 :motor (let ((motor (getf keys :motor)))
                          (unless (or (null motor) (integerp motor))
                            (error ":motor is a motor's part id, not ~s." motor))
                          motor))))
        (:wait (places 1)
         (checked-options step nil)
         (list :wait (check-number (first places) 0 150000 "Milliseconds")))
        (:wait-until
         (unless (= (length (rest step)) 1) (error "(wait-until condition) takes one condition."))
         (list :wait-until (checked-condition (second step))))
        (:follow-line (places 0)
         (let ((keys (checked-options step '(:until :speed :gain :target :sensor))))
           (unless (getf keys :until) (error "follow-line needs :until, the condition to stop on."))
           (list :follow-line
                 :speed (check-number (getf keys :speed 100) -1000 1000 ":speed")
                 :gain (check-number (getf keys :gain 1) -20 20 ":gain")
                 :target (check-number (getf keys :target 50) 0 100 ":target")
                 :until (checked-condition (getf keys :until))
                 :sensor (let ((sensor (getf keys :sensor)))
                           (unless (or (null sensor) (integerp sensor))
                             (error ":sensor is a sensor's part id, not ~s." sensor))
                           sensor))))
        (:repeat
         (let ((times (second step)))
           (unless (and (integerp times) (<= 1 times 100))
             (error "(repeat n steps...) repeats 1 to 100 times, not ~s." times))
           (list* :repeat times (mapcar #'checked-step (cddr step)))))
        (t (error "~(~s~) is not a step: the steps are ~{~(~a~)~^ ~}." name *step-names*))))))

(defun checked-program (program)
  (unless (listp program)
    (error "A program is a list of steps."))
  (let ((steps (mapcar #'checked-step program)))
    (unless (<= (count-steps steps) *program-limit*)
      (error "A program is at most ~a steps." *program-limit*))
    steps))

(defun read-program (text)
  "The program in text, whose steps may be written (straight 300) or (:straight 300)."
  (let ((*package* (find-package :keyword))
        (*read-eval* nil)
        (end (gensym)))
    (with-input-from-string (in text)
      (loop for form = (handler-case (read in nil end)
                         (end-of-file () (error "A step is missing a closing parenthesis."))
                         (reader-error () (error "That program cannot be read.")))
            until (eq form end)
            collect form))))

(defun number-text (x)
  "x as a program or Pybricks writes it: whole, or to two places."
  (if (< (abs (- x (round x))) 1d-9)
      (format nil "~d" (round x))
      (string-right-trim "0" (format nil "~,2f" x))))

(defun condition-text (condition)
  "A checked condition as a program writes it."
  (cond ((realp condition) (number-text condition))
        ((keywordp condition) (string-downcase condition))
        (t (format nil "(~(~a~)~{ ~a~})" (first condition) (mapcar #'condition-text (rest condition))))))

(defun step-text (step)
  "A checked step as a program writes it, without its parentheses."
  (flet ((text (x) (cond ((realp x) (number-text x))
                         ((consp x) (condition-text x))
                         (t (princ-to-string x)))))
    (if (member (first step) '(:wait-until :repeat))
        (format nil "~(~a~) ~a" (first step)
                (if (eql (first step) :repeat) (second step) (condition-text (second step))))
        (let* ((places (loop for x in (rest step) until (keywordp x) collect x))
               (keys (nthcdr (length places) (rest step))))
          (format nil "~(~a~)~{ ~a~}~{ ~(~s~) ~a~}" (first step) (mapcar #'text places)
                  (loop for (key value) on keys by #'cddr
                        when value append (list key (text value))))))))

;;
;; Sensing
;;

(defun find-sensor (robot kind id)
  (let ((sensor (find-if #'(lambda (s) (and (eql (sensor-kind s) kind) (or (null id) (eql (sensor-id s) id))))
                         (robot-sensors robot))))
    (or sensor
        (error (if id "Part ~a is not a ~(~a~)." "It has no ~*~(~a~).") id (substitute #\Space #\- (string kind))))))

(defun sensor-place (robot sensor)
  "Values: where sensor senses from on the table, x y z, and the way it looks, dx dy dz."
  (let ((rigid (robot-rigid robot)))
    (values (world-x rigid (sensor-x sensor) (sensor-y sensor))
            (world-y rigid (sensor-x sensor) (sensor-y sensor))
            (sensor-z sensor)
            (- (* (sensor-dx sensor) (rigid-cos rigid)) (* (sensor-dy sensor) (rigid-sin rigid)))
            (+ (* (sensor-dx sensor) (rigid-sin rigid)) (* (sensor-dy sensor) (rigid-cos rigid)))
            (sensor-dz sensor))))

(defun mat-brightness (rgb) (destructuring-bind (r g b) rgb (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b))))

(defun read-reflection (pilot robot sensor)
  "Percent, as the sensor's light comes back off the mat: its spot is a
few millimeters across, so an edge reads between its two sides, and it
reads weaker the higher it is."
  (multiple-value-bind (x y height) (sensor-place robot sensor)
    (let* ((spot (loop for (dx dy) in '((0 0) (4 0) (-4 0) (0 4) (0 -4))
                       sum (mat-brightness (mat-color (pilot-table pilot) (+ x dx) (+ y dy)))))
           (fading (min 1d0 (expt (/ 24 (max height 1d0)) 2))))
      (round (* 100 fading (/ spot 5))))))

(defun read-color (pilot robot sensor)
  (multiple-value-bind (x y height) (sensor-place robot sensor)
    (if (> height 40) :none (color-name (mat-color (pilot-table pilot) x y)))))

(defun ray-to-rigid (rigid x y dx dy)
  "How far along the ray from x y toward unit dx dy it meets rigid, or nil."
  (let (nearest)
    (dolist (shape (rigid-shapes rigid) nearest)
      (let ((hit
              (etypecase shape
                (circle-shape
                 (let* ((cx (- (world-x rigid (circle-shape-x shape) (circle-shape-y shape)) x))
                        (cy (- (world-y rigid (circle-shape-x shape) (circle-shape-y shape)) y))
                        (along (+ (* cx dx) (* cy dy)))
                        (miss (- (+ (* cx cx) (* cy cy)) (* along along)))
                        (r2 (expt (circle-shape-radius shape) 2)))
                   (when (and (<= miss r2) (> along 0))
                     (let ((hit (- along (sqrt (- r2 miss)))))
                       (and (>= hit 0) hit)))))
                (rect-shape
                 ;; the ray in the rectangle's own frame, through its slabs
                 (let* ((c (rigid-cos rigid)) (s (rigid-sin rigid))
                        (rx (- x (rigid-x rigid))) (ry (- y (rigid-y rigid)))
                        (ox (- (+ (* rx c) (* ry s)) (rect-shape-x shape)))
                        (oy (- (- (* ry c) (* rx s)) (rect-shape-y shape)))
                        (ux (+ (* dx c) (* dy s)))
                        (uy (- (* dy c) (* dx s)))
                        (low 0d0) (high most-positive-double-float))
                   (loop for (o u half) in (list (list ox ux (rect-shape-half-x shape))
                                                 (list oy uy (rect-shape-half-y shape)))
                         do (if (< (abs u) 1d-12)
                                (when (> (abs o) half) (setf low 1d0 high 0d0))
                                (let ((t1 (/ (- (- half) o) u)) (t2 (/ (- half o) u)))
                                  (setf low (max low (min t1 t2))
                                        high (min high (max t1 t2))))))
                   (and (<= low high) low))))))
        (when (and hit (or (null nearest) (< hit nearest)))
          (setf nearest hit))))))

(defun read-distance (robot sensor others)
  "Millimeters to the nearest thing the sensor looks at among others tall
enough to be seen, or 2000 when nothing is in range, as Pybricks answers."
  (multiple-value-bind (x y height dx dy dz) (sensor-place robot sensor)
    (cond ((< dz -0.9d0) (round height))
          ((> (abs dz) 0.5d0) 2000)
          (t (let* ((across (sqrt (+ (* dx dx) (* dy dy))))
                    (ux (/ dx across)) (uy (/ dy across))
                    (hits (loop for rigid in others
                                when (> (rigid-height rigid) height)
                                  append (let ((hit (ray-to-rigid rigid x y ux uy))) (and hit (list hit))))))
               (if hits (min 2000 (round (reduce #'min hits))) 2000))))))

(defun feel-forces! (pilot robot touches dt)
  "What each force sensor's button met this tick, in newtons."
  (setf (pilot-forces pilot)
        (loop for sensor in (robot-sensors robot)
              when (eql (sensor-kind sensor) :force-sensor)
                collect (multiple-value-bind (x y) (sensor-place robot sensor)
                          (cons (sensor-id sensor)
                                (min 10d0 (loop for touch in touches
                                                when (and (or (eq (touch-a touch) (robot-rigid robot))
                                                              (eq (touch-b touch) (robot-rigid robot)))
                                                          (< (+ (expt (- (touch-px touch) x) 2)
                                                                (expt (- (touch-py touch) y) 2))
                                                             100))
                                                  sum (/ (touch-pushed touch) dt 1000))))))))

(defun reading (name id pilot robot others)
  (ecase name
    (:reflection (read-reflection pilot robot (find-sensor robot :color-sensor id)))
    (:color (read-color pilot robot (find-sensor robot :color-sensor id)))
    (:distance (read-distance robot (find-sensor robot :distance-sensor id) others))
    ((:force :pressed)
     (let ((newtons (or (cdr (assoc (sensor-id (find-sensor robot :force-sensor id)) (pilot-forces pilot))) 0d0)))
       (if (eql name :force) newtons (>= newtons 3))))
    ;; clockwise, as Pybricks counts, and on past a turn without wrapping
    (:heading (* (/ 180 pi) (- (pilot-start-angle pilot) (rigid-angle (robot-rigid robot)))))
    (:driven (- (rolled robot) (pilot-start-rolled pilot)))))

(defun condition-value (condition pilot robot others)
  (flet ((value (x) (condition-value x pilot robot others)))
    (cond ((atom condition) condition)
          (t (destructuring-bind (name &rest arguments) condition
               (case name
                 (:and (every #'value arguments))
                 (:or (some #'value arguments))
                 (:not (not (value (first arguments))))
                 ((:< :> :<= :>=)
                  (let ((a (value (first arguments))) (b (value (second arguments))))
                    (unless (and (realp a) (realp b))
                      (error "(~(~a~) ...) compares numbers, not ~s and ~s." name a b))
                    (funcall (ecase name (:< #'<) (:> #'>) (:<= #'<=) (:>= #'>=)) a b)))
                 ((:= :/=)
                  (let* ((a (value (first arguments))) (b (value (second arguments)))
                         (same (if (and (realp a) (realp b)) (= a b) (eql a b))))
                    (if (eql name :=) same (not same))))
                 (t (reading name (first arguments) pilot robot others))))))))

;;
;; Driving
;;

(defun robot-wheel-drives (robot)
  (remove 0 (robot-drives robot) :key #'drive-wheels))

(defun rolled (robot)
  "The millimeters its wheels have counted, on the average."
  (let ((drives (robot-wheel-drives robot)))
    (/ (reduce #'+ drives :key #'drive-rolled) (length drives))))

(defun steer-sides! (robot left right dt)
  "Run each wheel motor so that its wheels' rims go left mm/s on the left
side and right on the right: what it should do, and a push toward it
from how fast it went in the last dt."
  (dolist (drive (robot-wheel-drives robot))
    (let ((wanted (ecase (drive-side drive) (-1 left) (1 right) (0 (/ (+ left right) 2))))
          (rate (/ (- (drive-rolled drive) (drive-last drive)) dt)))
      (setf (drive-last drive) (drive-rolled drive)
            (drive-power drive) (within (/ (+ wanted (* 0.5d0 (- wanted rate))) (drive-top drive)) 1d0)))))

(defun stop-sides! (robot)
  (dolist (drive (robot-wheel-drives robot))
    (setf (drive-last drive) (drive-rolled drive)
          (drive-power drive) 0d0)))

(defun trapezoid (distance speed acceleration)
  "Two values: a function of time answering the position and the speed
along a move of distance (signed), at most speed, speeding up and slowing
down at acceleration; and the time the move takes."
  (let* ((d (abs distance))
         (sign (if (minusp distance) -1d0 1d0))
         (v (float speed 1d0))
         (a (float acceleration 1d0))
         (ramp (/ v a))
         (ramp-distance (/ (* v v) (* 2 a))))
    ;; too short to reach speed: up, then straight down again
    (when (> (* 2 ramp-distance) d)
      (setf ramp (sqrt (/ d a))
            v (* a ramp)
            ramp-distance (/ d 2)))
    (let* ((cruise (if (plusp v) (/ (- d (* 2 ramp-distance)) v) 0d0))
           (total (+ ramp ramp cruise)))
      (values #'(lambda (time)
                  (let ((time (max 0d0 (min time total))))
                    (multiple-value-bind (s u)
                        (cond ((< time ramp)
                               (values (* a time time 0.5d0) (* a time)))
                              ((< time (+ ramp cruise))
                               (values (+ ramp-distance (* v (- time ramp))) v))
                              (t (let ((left (- total time)))
                                   (values (- d (* a left left 0.5d0)) (* a left)))))
                      (values (* sign s) (* sign u)))))
              total))))

(defun radians (degrees) (* degrees (/ pi 180)))

(defun default-pilot (robot heading table)
  "The settings Pybricks starts a drive base on, for this robot's motors and wheels."
  (let* ((top (reduce #'min (robot-wheel-drives robot) :key #'drive-top))
         (half (/ (or (robot-track robot) 100) 2))
         (acceleration (* 0.75d0 *motor-acceleration* (/ top *free-speed*))))
    (make-pilot :heading (float heading 1d0)
                :straight-speed (* 0.4d0 top)
                :straight-acceleration acceleration
                :turn-rate (/ (* (/ top half) (/ 180 pi)) 3)
                :turn-acceleration (* (/ acceleration half) (/ 180 pi))
                :table table
                :start-rolled (rolled robot)
                :start-angle (float heading 1d0))))

(defun drive-motion (robot pilot)
  "What keeps the robot going at the pilot's speed and turn rate, as a
Pybricks drive base's drive() does: both come up at the settings'
accelerations, and the robot follows where they would have taken it."
  (let* ((rigid (robot-rigid robot))
         (half (/ (robot-track robot) 2))
         (heading-ref (rigid-angle rigid))
         (start (rolled robot))
         (s-ref 0d0) (v 0d0) (w 0d0))
    #'(lambda (dt)
        (let ((up (* dt (pilot-straight-acceleration pilot)))
              (turn-up (* dt (radians (pilot-turn-acceleration pilot))))
              (spin (- (radians (pilot-rate pilot)))))
          (setf v (+ v (within (- (pilot-speed pilot) v) up))
                w (+ w (within (- spin w) turn-up))))
        (incf s-ref (* v dt))
        (incf heading-ref (* w dt))
        (let ((along (+ v (* 4 (- s-ref (- (rolled robot) start)))))
              (turning (+ w (* 4 (angle-difference heading-ref (rigid-angle rigid))))))
          (steer-sides! robot (- along (* turning half)) (+ along (* turning half)) dt)))))

(defun step-controller (step robot pilot others)
  "A function of dt that drives the robot through step, answering nil
until it is done, then :done, or :stalled when it never gets there.
others are the things on the table its distance sensors can see."
  (let* ((rigid (robot-rigid robot))
         (half (and (robot-track robot) (/ (robot-track robot) 2)))
         (elapsed 0d0)
         (gain 4d0))
    (labels ((off (target) (angle-difference target (rigid-angle rigid)))
             (both-sides ()
               (unless half (error "To ~(~a~) it needs a motor on each side." (first step))))
             (steer (along spin dt)
               ;; along mm/s forward, spin counterclockwise radians a second
               (steer-sides! robot (- along (* spin half)) (+ along (* spin half)) dt))
             (finish (outcome) (stop-sides! robot) outcome)
             (true-p (condition) (condition-value condition pilot robot others))
             (cruise (speed rate)
               (setf (pilot-speed pilot) speed (pilot-rate pilot) rate)
               (unless (pilot-motion pilot)
                 (setf (pilot-motion pilot) (drive-motion robot pilot))))
             (halt ()
               ;; where it got to is the heading from here on
               (setf (pilot-motion pilot) nil
                     (pilot-heading pilot) (rigid-angle rigid))
               (stop-sides! robot)))
      ;; a move of its own ends any drive still going
      (when (member (first step) '(:straight :turn :arc))
        (setf (pilot-motion pilot) nil))
      (ecase (first step)
        (:straight
         (let ((distance (second step))
               (start (rolled robot))
               (hold (pilot-heading pilot)))
           (multiple-value-bind (profile total)
               (trapezoid distance (pilot-straight-speed pilot) (pilot-straight-acceleration pilot))
             #'(lambda (dt)
                 (incf elapsed dt)
                 (multiple-value-bind (s-ref v-ref) (funcall profile elapsed)
                   (let ((s (- (rolled robot) start)))
                     (if half
                         (steer (+ v-ref (* gain (- s-ref s))) (* gain (off hold)) dt)
                         (let ((u (+ v-ref (* gain (- s-ref s))))) (steer-sides! robot u u dt)))
                     (cond ((and (>= elapsed total) (< (abs (- distance s)) 1.5d0)) (finish :done))
                           ((> elapsed (+ total 2)) (finish :stalled)))))))))
        (:turn
         (both-sides)
         (let* ((turn (radians (second step)))
                (start (pilot-heading pilot))
                (target (- start turn)))
           (setf (pilot-heading pilot) target)
           (multiple-value-bind (profile total)
               (trapezoid turn (radians (pilot-turn-rate pilot)) (radians (pilot-turn-acceleration pilot)))
             #'(lambda (dt)
                 (incf elapsed dt)
                 (multiple-value-bind (a-ref w-ref) (funcall profile elapsed)
                   ;; clockwise in the program, counterclockwise here
                   (steer 0d0 (+ (- w-ref) (* gain (off (- start a-ref)))) dt)
                   (cond ((and (>= elapsed total) (< (abs (off target)) (radians 0.5d0))) (finish :done))
                         ((> elapsed (+ total 2)) (finish :stalled))))))))
        (:arc
         (both-sides)
         (destructuring-bind (radius kind value) (rest step)
           (let* ((distance (if (eql kind :angle) (* (abs radius) (radians value)) value))
                  (start-heading (pilot-heading pilot))
                  (target (- start-heading (/ distance radius)))
                  (start (rolled robot)))
             (setf (pilot-heading pilot) target)
             (multiple-value-bind (profile total)
                 (trapezoid distance (pilot-straight-speed pilot) (pilot-straight-acceleration pilot))
               #'(lambda (dt)
                   (incf elapsed dt)
                   (multiple-value-bind (s-ref v-ref) (funcall profile elapsed)
                     (let* ((s (- (rolled robot) start))
                            (along (+ v-ref (* gain (- s-ref s))))
                            (heading-ref (- start-heading (/ s-ref radius))))
                       ;; a radius to the right turns clockwise
                       (steer along (+ (- (/ along radius)) (* gain (off heading-ref))) dt)
                       (cond ((and (>= elapsed total) (< (abs (- distance s)) 1.5d0)
                                   (< (abs (off target)) (radians 1d0)))
                              (finish :done))
                             ((> elapsed (+ total 2)) (finish :stalled))))))))))
        (:drive
         (both-sides)
         (destructuring-bind (speed rate &optional milliseconds) (rest step)
           (cruise speed rate)
           (if milliseconds
               #'(lambda (dt)
                   (incf elapsed dt)
                   (when (>= (* 1000 elapsed) milliseconds)
                     (halt)
                     :done))
               #'(lambda (dt) (declare (ignore dt)) :done))))
        (:stop
         #'(lambda (dt) (declare (ignore dt)) (halt) :done))
        (:settings
         (loop for (key value) on (rest step) by #'cddr
               do (ecase key
                    (:straight-speed (setf (pilot-straight-speed pilot) value))
                    (:straight-acceleration (setf (pilot-straight-acceleration pilot) value))
                    (:turn-rate (setf (pilot-turn-rate pilot) value))
                    (:turn-acceleration (setf (pilot-turn-acceleration pilot) value))))
         #'(lambda (dt) (declare (ignore dt)) :done))
        (:wait
         (let ((milliseconds (second step)))
           #'(lambda (dt)
               (incf elapsed dt)
               (when (>= (* 1000 elapsed) milliseconds) :done))))
        (:wait-until
         (let ((condition (second step)))
           #'(lambda (dt)
               (declare (ignore dt))
               (when (true-p condition) :done))))
        (:follow-line
         (both-sides)
         (destructuring-bind (&key speed gain target until sensor) (rest step)
           (let ((eye (find-sensor robot :color-sensor sensor)))
             #'(lambda (dt)
                 (declare (ignore dt))
                 (cond ((true-p until) (halt) :done)
                       (t (cruise speed (* gain (- (read-reflection pilot robot eye) target)))
                          nil))))))
        (:arm
         (destructuring-bind (degrees &key speed motor) (rest step)
           (let ((arm (find-if #'(lambda (a) (or (null motor) (eql (drive-id (arm-drive a)) motor)))
                               (robot-arms robot))))
             (unless arm
               (error (if motor "Motor ~a turns no arm." "It has no motor turning an arm.") motor))
             (incf (arm-target arm) (radians degrees))
             (setf (arm-speed arm) (min 1d0 (/ (radians speed) (* *free-speed* (abs (arm-ratio arm))))))
             (let ((limit (+ 2d0 (* 3 (/ (abs degrees) speed)))))
               #'(lambda (dt)
                   (incf elapsed dt)
                   (cond ((< (abs (- (arm-target arm) (arm-angle arm))) (radians 1d0)) :done)
                         ((> elapsed limit)
                          ;; it stays where it got to, not straining at the goal
                          (setf (arm-target arm) (arm-angle arm))
                          :stalled)))))))))))

;;
;; A run
;;

(defun run-program (robot program &key (table *table*))
  "Run program, already checked, on the table.  A plist: :steps, each with
how it ended and when; :robot where it stopped; :pieces where they
stopped; :frames for the page."
  (let* ((pilot (default-pilot robot (place-robot! robot table) table))
         (rigid (robot-rigid robot))
         (pieces (table-pieces table))
         (others (append pieces (table-walls table)
                         (mapcar #'(lambda (f) (apply #'fixed-rigid f)) (getf table :fixed))))
         (rigids (cons rigid others))
         (treads (robot-treads robot))
         (seconds 0d0)
         (tick 0)
         (taken 0)
         (controller nil)
         (left program)
         (current nil)
         (frames nil)
         (ended nil)
         (quiet 0d0))
    (when (touches-of (list* rigid (nthcdr (length pieces) others)))
      (error "It does not fit on the table's start."))
    (loop
      ;; the next step; a repeat lays out its steps that many times
      (loop while (and (null controller) left)
            do (setf current (pop left))
               (if (eql (first current) :repeat)
                   (setf left (append (loop repeat (second current) append (cddr current)) left))
                   (progn
                     (when (> (incf taken) *step-limit*)
                       (error "The program takes more than ~a steps." *step-limit*))
                     (setf controller (step-controller current robot pilot others)))))
      (when controller
        (let ((outcome (funcall controller *tick*)))
          (when outcome
            (push (list current outcome (/ (round (* 10 seconds)) 10d0)) ended)
            (setf controller nil))))
      (when (pilot-motion pilot) (funcall (pilot-motion pilot) *tick*))
      (dolist (tread treads) (roll-tread! rigid tread *tick*))
      (dolist (arm (robot-arms robot)) (swing-arm! arm robot *tick*))
      (when (robot-arms robot) (pose-arms! robot))
      (let ((touches (step-rigids! rigids *tick*)))
        (feel-touches! robot touches *tick*)
        (feel-forces! pilot robot touches *tick*))
      (incf seconds *tick*)
      (when (zerop (mod tick *frame-ticks*))
        (push (frame-of seconds robot pieces) frames))
      (incf tick)
      ;; a program done, the table comes to rest
      (when (and (null controller) (null left))
        (when (pilot-motion pilot)
          (setf (pilot-motion pilot) nil)
          (stop-sides! robot))
        (incf quiet *tick*)
        (when (or (> quiet 4d0) (and (> quiet 0.3d0) (still-p (cons rigid pieces))))
          (return)))
      (when (>= seconds *match-seconds*)
        (return)))
    (push (frame-of seconds robot pieces) frames)
    (list :steps (reverse ended)
          :over (and (or controller left) t)
          :seconds (/ (round (* 10 seconds)) 10d0)
          :robot (list (round (rigid-x rigid)) (round (rigid-y rigid)))
          :heading (robot-heading robot)
          :pieces (mapcar #'(lambda (p)
                              (list :piece (first (rigid-name p)) :index (second (rigid-name p))
                                    :at (list (round (rigid-x p)) (round (rigid-y p)))
                                    :ring (ring-of p table)))
                          pieces)
          :frames (coerce (reverse frames) 'vector))))

(defun robot-heading (robot)
  "Whole degrees the robot faces from the length of the table, clockwise
as Pybricks counts a heading."
  (let ((forward (robot-forward robot)))
    (round (* (/ 180 pi)
              (- (angle-difference (+ (rigid-angle (robot-rigid robot)) (atan (second forward) (first forward)))
                                   0))))))

(defun step-line (entry)
  "What became of a step of a run, in words."
  (destructuring-bind (step outcome seconds) entry
    (format nil "~a: ~(~a~) at ~,1f s" (step-text step) outcome seconds)))

(defun run-summary (result)
  "List of lines: where a run left the robot and the pieces."
  (let ((rung (remove nil (getf result :pieces) :key #'(lambda (p) (getf p :ring)))))
    (append (list (format nil "Stopped at ~{~a, ~a~} facing ~a after ~,1f s."
                          (getf result :robot) (getf result :heading) (getf result :seconds)))
            (mapcar #'(lambda (p)
                        (format nil "~:(~a~) ~a is in the ~(~a~) ring."
                                (getf p :piece) (getf p :index) (getf p :ring)))
                    rung)
            (when (getf result :over) (list "The match ended first.")))))
