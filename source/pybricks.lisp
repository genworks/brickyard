;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The program a robot runs on the table, written out for a real hub as a
;; Pybricks program (pybricks.com, MicroPython on LEGO hubs).  The steps
;; are Pybricks' already (pilot.lisp); what the build adds is the drive
;; base's measurements, which way each motor must turn, its gearing, and
;; its sensors:
;;
;;   - the left wheel motor goes on port A, the right on B, arm motors on
;;     C onward in the order the build has them, then the sensors;
;;   - wheel_diameter and axle_track are the build's own;
;;   - Pybricks counts a motor's turn as seen looking at its shaft, so
;;     each motor's Direction is the one that makes positive drive the
;;     robot forward, or turn an arm about its shaft's axis the way a
;;     positive (arm ...) does here;
;;   - a train between motor and wheel or arm becomes gears=[a, b], the
;;     motor's side first, so Pybricks counts at the wheel or the arm;
;;   - a color sensor is a ColorSensor, a distance sensor an
;;     UltrasonicSensor (Pybricks' class for it), a force sensor a
;;     ForceSensor.
;;
;; The hub is taken to lie flat, buttons up, facing the way the robot
;; drives; one mounted otherwise needs PrimeHub(top_side=..., front_side=...).
;;

(defparameter *sensor-classes*
  '((:color-sensor "ColorSensor" "color_sensor")
    (:distance-sensor "UltrasonicSensor" "distance_sensor")
    (:force-sensor "ForceSensor" "force_sensor"))
  "List of (type class variable): each sensor as a Pybricks program names it.")

(defun gears-of (ratio)
  "Pybricks' gears for a train whose output turns ratio times the motor, or nil for none."
  (let ((ratio (rational (abs ratio))))
    (unless (= ratio 1)
      (list (numerator ratio) (denominator ratio)))))

(defun python-number (x) (number-text x))

(defun motor-line (variable port drive)
  (format nil "~a = Motor(Port.~a, Direction.~:[COUNTERCLOCKWISE~;CLOCKWISE~]~@[, gears=[~{~d~^, ~}]~])"
          variable port (drive-clockwise drive) (gears-of (drive-ratio drive))))

(defun pybricks-names (robot)
  "List of (device variable port): which motor or sensor is which in the
program, the device a drive or a sensor."
  (let* ((wheels (robot-wheel-drives robot))
         (left (remove -1 wheels :key #'drive-side :test-not #'eql))
         (right (remove 1 wheels :key #'drive-side :test-not #'eql))
         (arms (remove-duplicates (mapcar #'arm-drive (robot-arms robot))))
         (sensors (robot-sensors robot)))
    (unless (and (= (length left) 1) (= (length right) 1) (= (length wheels) 2))
      (error "A Pybricks drive base takes one wheel motor on each side; this build has ~a on the left, ~a on the right and ~a in the middle."
             (length left) (length right) (- (length wheels) (length left) (length right))))
    (when (> (+ 2 (length arms) (length sensors)) 6)
      (error "A hub has six ports, and this build has ~a motors and sensors." (+ 2 (length arms) (length sensors))))
    (let ((ports (coerce "CDEF" 'list)))
      (append (list (list (first left) "left_motor" "A") (list (first right) "right_motor" "B"))
              (loop for drive in arms
                    for port = (pop ports)
                    collect (list drive (format nil "motor_~(~a~)" port) (string port)))
              (loop for sensor in sensors
                    for port = (pop ports)
                    for base = (third (assoc (sensor-kind sensor) *sensor-classes*))
                    for count = (count (sensor-kind sensor) sensors :key #'sensor-kind)
                    collect (list sensor (if (> count 1) (format nil "~a_~(~a~)" base port) base) (string port)))))))

(defun sensor-variable (names kind id)
  (let ((entry (find-if #'(lambda (e) (and (sensor-p (first e)) (eql (sensor-kind (first e)) kind)
                                           (or (null id) (eql (sensor-id (first e)) id))))
                        names)))
    (unless entry
      (error (if id "Part ~a is not a ~(~a~)." "It has no ~*~(~a~).") id (substitute #\Space #\- (string kind))))
    (second entry)))

(defun python-condition (condition names)
  "A checked condition as a Python expression."
  (flet ((sub (c) (python-condition c names)))
    (cond ((realp condition) (python-number condition))
          ((keywordp condition) (format nil "Color.~a" (symbol-name condition)))
          (t (destructuring-bind (name &rest arguments) condition
               (case name
                 ((:and :or) (format nil "(~{~a~^ ~(~a~) ~})"
                                     (butlast (loop for a in arguments append (list (sub a) name)))))
                 (:not (format nil "not ~a" (sub (first arguments))))
                 ((:< :> :<= :>= := :/=)
                  (format nil "~a ~a ~a" (sub (first arguments))
                          (ecase name (:< "<") (:> ">") (:<= "<=") (:>= ">=") (:= "==") (:/= "!="))
                          (sub (second arguments))))
                 (:heading "hub.imu.heading()")
                 (:driven "drive_base.distance()")
                 (t (format nil "~a.~(~a~)()"
                            (sensor-variable names (cdr (assoc name *readings*)) (first arguments))
                            name))))))))

(defun pybricks-lines (step names)
  "The lines of Python for one checked step, a repeat's indented."
  (let ((n #'python-number))
    (ecase (first step)
      (:straight (list (format nil "drive_base.straight(~a)" (funcall n (second step)))))
      (:turn (list (format nil "drive_base.turn(~a)" (funcall n (second step)))))
      (:arc (destructuring-bind (radius kind value) (rest step)
              (list (format nil "drive_base.arc(~a, ~(~a~)=~a)" (funcall n radius) kind (funcall n value)))))
      (:drive (destructuring-bind (speed rate &optional milliseconds) (rest step)
                (append (list (format nil "drive_base.drive(~a, ~a)" (funcall n speed) (funcall n rate)))
                        (when milliseconds
                          (list (format nil "wait(~a)" (funcall n milliseconds))
                                "drive_base.stop()")))))
      (:stop (list "drive_base.stop()"))
      (:settings (list (format nil "drive_base.settings(~{~a~^, ~})"
                               (loop for (key value) on (rest step) by #'cddr
                                     collect (format nil "~(~a~)=~a" (substitute #\_ #\- (string key))
                                                     (funcall n value))))))
      (:arm (destructuring-bind (degrees &key speed motor) (rest step)
              (let ((entry (find-if #'(lambda (e) (and (drive-p (first e))
                                                       (not (member (second e) '("left_motor" "right_motor")
                                                                    :test #'string=))
                                                       (or (null motor) (eql (drive-id (first e)) motor))))
                                    names)))
                (unless entry
                  (error (if motor "Motor ~a turns no arm." "It has no motor turning an arm.") motor))
                (list (format nil "~a.run_angle(~a, ~a)" (second entry) (funcall n speed) (funcall n degrees))))))
      (:wait (list (format nil "wait(~a)" (funcall n (second step)))))
      (:wait-until (list (format nil "while not (~a):" (python-condition (second step) names))
                         "    wait(10)"))
      (:follow-line
       (destructuring-bind (&key speed gain target until sensor) (rest step)
         (list (format nil "while not (~a):" (python-condition until names))
               (format nil "    drive_base.drive(~a, (~a.reflection() - ~a) * ~a)"
                       (funcall n speed) (sensor-variable names :color-sensor sensor)
                       (funcall n target) (funcall n gain))
               "    wait(10)"
               "drive_base.stop()")))
      (:repeat (cons (format nil "for i in range(~a):" (second step))
                     (mapcar #'(lambda (line) (format nil "    ~a" line))
                             (or (loop for inner in (cddr step) append (pybricks-lines inner names))
                                 (list "pass"))))))))

(defun program-colors-p (program)
  "True when program names a color, so the Python needs Color."
  (labels ((names-color-p (x)
             (cond ((member x *color-names*) t)
                   ((consp x) (some #'names-color-p x)))))
    (names-color-p program)))

(defun program-text (program &optional (indent ""))
  "The program's lines as comments, a repeat's steps beneath it."
  (loop for step in program
        append (cons (format nil "#   ~a(~a)" indent (step-text step))
                     (when (eql (first step) :repeat)
                       (program-text (cddr step) (concatenate 'string indent "  "))))))

(defun pybricks-program (robot program &key (title "A Brickyard program"))
  "Program, already checked, for robot, as the text of a Pybricks program."
  (let* ((names (pybricks-names robot))
         (classes (remove-duplicates (mapcar #'(lambda (s) (second (assoc (sensor-kind s) *sensor-classes*)))
                                             (robot-sensors robot))
                                     :test #'string=)))
    (with-output-to-string (s)
      (format s "# ~a, written by Brickyard: change the program there and write it out again.~%" title)
      (format s "#~%")
      (format s "~{~a~%~}" (program-text program))
      (format s "#~%# Ports: ~{~a~^, ~}.~%"
              (mapcar #'(lambda (e) (format nil "~a on ~a" (second e) (third e))) names))
      (format s "# The hub lies flat, buttons up, facing the way the robot drives.~%~%")
      (format s "from pybricks.hubs import PrimeHub~%")
      (format s "from pybricks.parameters import ~:[~;Color, ~]Direction, Port~%" (program-colors-p program))
      (format s "from pybricks.pupdevices import ~{~a~^, ~}~%"
              (cons "Motor" (sort (copy-list classes) #'string<)))
      (format s "from pybricks.robotics import DriveBase~%")
      (format s "from pybricks.tools import wait~%~%")
      (format s "hub = PrimeHub()~%")
      (dolist (entry names)
        (destructuring-bind (device variable port) entry
          (format s "~a~%"
                  (if (drive-p device)
                      (motor-line variable port device)
                      (format nil "~a = ~a(Port.~a)" variable
                              (second (assoc (sensor-kind device) *sensor-classes*)) port)))))
      (format s "drive_base = DriveBase(left_motor, right_motor, wheel_diameter=~a, axle_track=~a)~%"
              (python-number (robot-wheel-diameter robot)) (python-number (robot-track robot)))
      (format s "drive_base.use_gyro(True)~%~%")
      (dolist (step program)
        (dolist (line (pybricks-lines step names))
          (format s "~a~%" line))))))
