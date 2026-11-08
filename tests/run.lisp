;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;
;; Brickyard's checks of the table: the robot drives where its program
;; says, pushes what it meets, and writes the same program out for a real
;; hub.  Load Brickyard, then this file, then (brickyard::run-tests),
;; which prints a line a check and answers (passed failed).  Nothing here
;; is particular to one Lisp: the same numbers should come out of each.
;;

(in-package :brickyard)

(defvar *checks* nil "List of (name passed detail), newest first.")

(defun check (name passed &optional detail)
  (push (list name (and passed t) detail) *checks*)
  (format t "~&~:[FAIL~;pass~] ~a~@[  ~s~]~%" passed name (unless passed detail))
  passed)

(defun test-robot (specs)
  "The robot specs make, through a build of its own."
  (let ((id (bt:with-lock-held (*builds-lock*) (make-build :specs specs))))
    (unwind-protect (bt:with-lock-held (*builds-lock*) (robot-of (find-build id)))
      (ignore-errors (delete-file (build-file id)))
      (remhash id *builds*))))

(defun bare-table ()
  (let ((table (copy-list *table*)))
    (setf (getf table :pieces) nil (getf table :fixed) nil)
    table))

(defun drive-test (program &key (specs *robot-build*) (table (bare-table)))
  (run-program (test-robot specs) (checked-program program) :table table))

(defun near (a b tolerance) (<= (abs (- a b)) tolerance))

(defun piece-at (result index) (getf (find index (getf result :pieces) :key #'(lambda (p) (getf p :index))) :at))

(defun test-driving ()
  (destructuring-bind (x y) (getf (drive-test '((:straight 500))) :robot)
    (check "straight 500 goes 500 along the table" (and (near x 690 3) (near y 571 2)) (list x y)))
  (destructuring-bind (x y) (getf (drive-test '((:straight 400) (:straight -400))) :robot)
    (check "out and back comes back" (and (near x 190 3) (near y 571 2)) (list x y)))
  (let ((result (drive-test '((:turn 90)))))
    (check "turn 90 turns clockwise, as Pybricks does" (near (getf result :heading) 90 1) (getf result :heading)))
  (let ((result (drive-test '((:straight 300) (:turn 90) (:straight 300) (:turn 90)
                              (:straight 300) (:turn 90) (:straight 300) (:turn 90)))))
    (destructuring-bind (x y) (getf result :robot)
      (check "a square comes home" (and (near x 190 6) (near y 571 6) (near (getf result :heading) 0 1))
             (list x y (getf result :heading)))))
  ;; to the right of a robot facing along the table is toward y = 0: the
  ;; circle's center is 300 that way, and a quarter of it ends facing 90
  (let ((result (drive-test '((:arc 300 :angle 90)))))
    (destructuring-bind (x y) (getf result :robot)
      (check "arc 300 :angle 90 ends a quarter circle to the right"
             (and (near x 490 12) (near y 271 12) (near (getf result :heading) 90 2))
             (list x y (getf result :heading)))))
  (let* ((slow (drive-test '((:settings :straight-speed 60) (:straight 300))))
         (fast (drive-test '((:straight 300)))))
    (check "settings :straight-speed slows a straight"
           (> (third (second (getf slow :steps))) (+ 1 (third (first (getf fast :steps)))))
           (list (getf slow :steps) (getf fast :steps))))
  (let ((result (drive-test '((:wait 1500)))))
    (check "wait counts milliseconds" (near (third (first (getf result :steps))) 1.5 0.1) (getf result :steps)))
  (destructuring-bind (x y) (getf (drive-test '((:drive 150 0 2000))) :robot)
    (check "drive 150 0 2000 goes about 300" (and (near x 490 40) (near y 571 5)) (list x y)))
  (destructuring-bind (x y) (getf (drive-test '((:straight 3000))) :robot)
    (check "a wall stops it" (and (<= x 2340) (> x 2300)) (list x y))))

(defun test-pieces ()
  (let ((result (run-program (test-robot *robot-build*) (checked-program (read-program *sample-program*)))))
    (check "the sample program puts balls 0 and 3 in the red ring"
           (equal (mapcar #'(lambda (i) (getf (find i (getf result :pieces) :key #'(lambda (p) (getf p :index))) :ring))
                          '(0 3))
                  '(:red :red))
           (getf result :pieces)))
  (flet ((ball-2 (program) (piece-at (run-program (test-robot *arm-build*) (checked-program program)) 2)))
    (let ((lowered (ball-2 '((:turn 21) (:straight 547) (:turn -21) (:arm -25) (:straight 600))))
          (raised (ball-2 '((:turn 21) (:straight 547) (:turn -21) (:arm 30) (:straight 200)))))
      (check "a lowered arm sweeps a ball" (> (first lowered) 1030) lowered)
      (check "a raised arm passes over it" (equal raised '(1000 320)) raised)))
  (let ((result (run-program (test-robot *arm-build*) (checked-program '((:arm -80))))))
    (check "the mat stops an arm going down" (eql (second (first (getf result :steps))) :stalled)
           (getf result :steps))))

(defun refusal (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(defun test-refusals ()
  (check "a step that is not one"
         (search "is not a step" (or (refusal #'(lambda () (checked-program (read-program "(fly 100)")))) "")))
  (check "a distance that is not a number"
         (search "must be a number" (or (refusal #'(lambda () (checked-program (read-program "(straight \"far\")")))) "")))
  (check "an arc wants :angle or :distance"
         (search "one of them" (or (refusal #'(lambda () (checked-program '((:arc 300))))) "")))
  (check "a program cannot read-evaluate"
         (refusal #'(lambda () (read-program "#.(print 1)"))))
  (check "no motor, no driving"
         (search "No motor" (or (refusal #'(lambda () (test-robot *sample-build*))) "")))
  (check "a gearbox does not stand"
         (search "tip over" (or (refusal #'(lambda () (test-robot *gearbox-build*))) ""))))

(defun test-pybricks ()
  (let* ((robot (test-robot *robot-build*))
         (text (pybricks-program robot (checked-program (read-program "(straight 500) (turn 90) (arc 300 :angle 90) (drive 150 0 2000) (settings :straight-speed 200) (wait 1000)")))))
    (dolist (line '("left_motor = Motor(Port.A, Direction.COUNTERCLOCKWISE)"
                    "right_motor = Motor(Port.B, Direction.CLOCKWISE)"
                    "drive_base = DriveBase(left_motor, right_motor, wheel_diameter=40, axle_track=120)"
                    "drive_base.use_gyro(True)"
                    "drive_base.straight(500)"
                    "drive_base.turn(90)"
                    "drive_base.arc(300, angle=90)"
                    "drive_base.drive(150, 0)"
                    "drive_base.settings(straight_speed=200)"
                    "wait(1000)"))
      (check (format nil "Python: ~a" line) (search line text) text)))
  (let ((text (pybricks-program (test-robot *arm-build*) (checked-program '((:arm -25) (:arm 25 :speed 300))))))
    (check "Python: the arm's motor on C, turning the way a positive arm does"
           (search "motor_c = Motor(Port.C, Direction.COUNTERCLOCKWISE)" text) text)
    (check "Python: arm steps run the arm's motor" (and (search "motor_c.run_angle(200, -25)" text)
                                                        (search "motor_c.run_angle(300, 25)" text))
           text))
  (check "Python wants a wheel motor each side"
         (search "one wheel motor on each side"
                 (or (refusal #'(lambda ()
                                  (let ((robot (test-robot *robot-build*)))
                                    ;; both wheel motors on the left
                                    (dolist (drive (robot-drives robot)) (setf (drive-side drive) -1))
                                    (pybricks-program robot nil))))
                     ""))))

(defun sensor-test (text)
  "Where a run of text leaves the sensor robot, its heading, and its color sensor."
  (let* ((robot (test-robot *sensor-build*))
         (result (run-program robot (checked-program (read-program text)) :table (bare-table))))
    (multiple-value-bind (x y) (sensor-place robot (find :color-sensor (robot-sensors robot) :key #'sensor-kind))
      (append (getf result :robot) (list (getf result :heading) (round x) (round y))))))

(defun test-sensors ()
  ;; the vertical line's near edge is x = 1200, from y 581 up; the
  ;; robot's color sensor is 32 mm to its left, 13 mm ahead
  (destructuring-bind (x y heading sx sy) (sensor-test "(drive 150 0) (wait-until (< (reflection) 30)) (stop)")
    (declare (ignore x y heading sy))
    (check "wait-until a dark reflection stops on a line" (<= 1200 sx 1222) sx))
  (destructuring-bind (x &rest rest) (sensor-test "(drive 200 0) (wait-until (< (distance) 150)) (stop)")
    (declare (ignore rest))
    ;; its distance sensor is 21 mm ahead of its middle; the wall at 2362
    (check "wait-until a near distance stops short of the wall" (<= 2180 x 2200) x))
  (destructuring-bind (x &rest rest) (sensor-test "(drive 150 0) (wait-until (pressed)) (stop)")
    (declare (ignore rest))
    (check "wait-until pressed stops with the button on the wall" (<= 2325 x 2340) x))
  (destructuring-bind (x y heading sx sy) (sensor-test "(drive 150 0) (wait-until (= (color) red)) (stop)")
    (declare (ignore x y heading))
    ;; the red ring's outer edge is 180 from (1900 571)
    (check "wait-until red stops on the red ring"
           (< 170 (sqrt (+ (expt (- sx 1900) 2) (expt (- sy 571) 2))) 182) (list sx sy)))
  (destructuring-bind (x y heading sx sy) (sensor-test "(drive 150 -45) (wait-until (<= (heading) -90)) (stop)")
    (declare (ignore x y sx sy))
    (check "the heading counts as Pybricks', counterclockwise negative" (<= -93 heading -87) heading))
  (destructuring-bind (x y heading sx sy) (sensor-test "(straight 300) (follow-line :until (> (driven) 1200))")
    (declare (ignore y sx))
    (check "follow-line keeps the sensor on the line's edge"
           (and (> x 1250) (<= 576 sy 586) (<= -3 heading 3)) (list x sy heading)))
  (destructuring-bind (x y heading &rest rest) (sensor-test "(repeat 4 (straight 200) (turn 90))")
    (declare (ignore rest))
    (check "repeat 4 of a side and a turn comes home" (and (near x 190 6) (near y 571 6) (near heading 0 1))
           (list x y heading)))
  (check "a condition must compare something"
         (search "compares two things" (or (refusal #'(lambda () (checked-program '((:wait-until (:< (:reflection))))))) "")))
  (check "a reading is of a sensor the robot has"
         (search "no force sensor"
                 (or (refusal #'(lambda () (run-program (test-robot *robot-build*)
                                                        (checked-program '((:wait-until (:pressed))))
                                                        :table (bare-table))))
                     "")))
  (let ((text (pybricks-program (test-robot *sensor-build*)
                                (checked-program (read-program "(drive 150 0) (wait-until (or (< (reflection) 30) (= (color) red))) (stop) (repeat 2 (straight 100) (turn 90)) (follow-line :until (pressed) :gain 0.8) (wait-until (< (distance) 200))")))))
    (dolist (line '("from pybricks.parameters import Color, Direction, Port"
                    "from pybricks.pupdevices import Motor, ColorSensor, ForceSensor, UltrasonicSensor"
                    "distance_sensor = UltrasonicSensor(Port.C)"
                    "color_sensor = ColorSensor(Port.D)"
                    "force_sensor = ForceSensor(Port.E)"
                    "while not ((color_sensor.reflection() < 30 or color_sensor.color() == Color.RED)):"
                    "for i in range(2):"
                    "    drive_base.straight(100)"
                    "while not (force_sensor.pressed()):"
                    "    drive_base.drive(100, (color_sensor.reflection() - 50) * 0.8)"
                    "while not (distance_sensor.distance() < 200):"))
      (check (format nil "Python: ~a" line) (search line text) text))))

(defun fingerprint ()
  "Where a long run leaves everything, for comparing one Lisp with another."
  (let ((result (run-program (test-robot *arm-build*)
                             (checked-program (read-program "(turn 21) (straight 547) (turn -21) (arm -25) (straight 600) (arm 25) (arc -400 :angle 60) (straight -300)")))))
    (list (getf result :robot) (getf result :heading) (getf result :seconds)
          (mapcar #'(lambda (p) (getf p :at)) (getf result :pieces))
          (sensor-test "(straight 300) (follow-line :until (> (driven) 1200)) (turn 90) (drive 150 0) (wait-until (or (pressed) (< (distance) 120))) (stop)"))))

(defun run-tests ()
  (setf *checks* nil)
  (test-driving)
  (test-pieces)
  (test-refusals)
  (test-pybricks)
  (test-sensors)
  (format t "~&fingerprint ~s~%" (fingerprint))
  (let ((failed (count nil *checks* :key #'second)))
    (format t "~&~a passed, ~a failed~%" (- (length *checks*) failed) failed)
    (list (- (length *checks*) failed) failed)))
