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
;; base's measurements, which way each motor must turn, and its gearing:
;;
;;   - the left wheel motor goes on port A, the right on B, arm motors on C
;;     to F in the order the build has them;
;;   - wheel_diameter and axle_track are the build's own;
;;   - Pybricks counts a motor's turn as seen looking at its shaft, so
;;     each motor's Direction is the one that makes positive drive the
;;     robot forward, or turn an arm about its shaft's axis the way a
;;     positive (arm ...) does here;
;;   - a train between motor and wheel or arm becomes gears=[a, b], the
;;     motor's side first, so Pybricks counts at the wheel or the arm.
;;
;; The hub is taken to lie flat, buttons up, facing the way the robot
;; drives; one mounted otherwise needs PrimeHub(top_side=..., front_side=...).
;;

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
  "List of (drive variable port): which motor is which in the program."
  (let* ((wheels (robot-wheel-drives robot))
         (left (remove -1 wheels :key #'drive-side :test-not #'eql))
         (right (remove 1 wheels :key #'drive-side :test-not #'eql))
         (arms (remove-duplicates (mapcar #'arm-drive (robot-arms robot)))))
    (unless (and (= (length left) 1) (= (length right) 1) (= (length wheels) 2))
      (error "A Pybricks drive base takes one wheel motor on each side; this build has ~a on the left, ~a on the right and ~a in the middle."
             (length left) (length right) (- (length wheels) (length left) (length right))))
    (when (> (length arms) 4)
      (error "A hub has six ports: two for the wheels, four for anything else."))
    (append (list (list (first left) "left_motor" "A") (list (first right) "right_motor" "B"))
            (loop for drive in arms
                  for port across "CDEF"
                  collect (list drive (format nil "motor_~(~a~)" port) port)))))

(defun pybricks-step (step names)
  "The lines of Python for one checked step."
  (let ((n #'python-number))
    (ecase (first step)
      (:straight (list (format nil "drive_base.straight(~a)" (funcall n (second step)))))
      (:turn (list (format nil "drive_base.turn(~a)" (funcall n (second step)))))
      (:arc (destructuring-bind (radius kind value) (rest step)
              (list (format nil "drive_base.arc(~a, ~(~a~)=~a)" (funcall n radius) kind (funcall n value)))))
      (:drive (destructuring-bind (speed rate milliseconds) (rest step)
                (list (format nil "drive_base.drive(~a, ~a)" (funcall n speed) (funcall n rate))
                      (format nil "wait(~a)" (funcall n milliseconds))
                      "drive_base.stop()")))
      (:settings (list (format nil "drive_base.settings(~{~a~^, ~})"
                               (loop for (key value) on (rest step) by #'cddr
                                     collect (format nil "~(~a~)=~a" (substitute #\_ #\- (string key))
                                                     (funcall n value))))))
      (:arm (destructuring-bind (degrees &key speed motor) (rest step)
              (let ((entry (find-if #'(lambda (e) (and (not (member (second e) '("left_motor" "right_motor")
                                                                    :test #'string=))
                                                       (or (null motor) (eql (drive-id (first e)) motor))))
                                    names)))
                (unless entry
                  (error (if motor "Motor ~a turns no arm." "It has no motor turning an arm.") motor))
                (list (format nil "~a.run_angle(~a, ~a)" (second entry) (funcall n speed) (funcall n degrees))))))
      (:wait (list (format nil "wait(~a)" (funcall n (second step))))))))

(defun pybricks-program (robot program &key (title "A Brickyard program"))
  "Program, already checked, for robot, as the text of a Pybricks program."
  (let ((names (pybricks-names robot)))
    (with-output-to-string (s)
      (format s "# ~a, written by Brickyard: change the program there and write it out again.~%" title)
      (format s "#~%")
      (dolist (step program) (format s "#   (~a)~%" (step-text step)))
      (format s "#~%# Ports: ~{~a~^, ~}.~%"
              (mapcar #'(lambda (e) (format nil "~a on ~a" (second e) (third e))) names))
      (format s "# The hub lies flat, buttons up, facing the way the robot drives.~%~%")
      (format s "from pybricks.hubs import PrimeHub~%")
      (format s "from pybricks.parameters import Direction, Port~%")
      (format s "from pybricks.pupdevices import Motor~%")
      (format s "from pybricks.robotics import DriveBase~%")
      (format s "from pybricks.tools import wait~%~%")
      (format s "hub = PrimeHub()~%")
      (dolist (entry names)
        (destructuring-bind (drive variable port) entry
          (format s "~a~%" (motor-line variable port drive))))
      (format s "drive_base = DriveBase(left_motor, right_motor, wheel_diameter=~a, axle_track=~a)~%"
              (python-number (robot-wheel-diameter robot)) (python-number (robot-track robot)))
      (format s "drive_base.use_gyro(True)~%~%")
      (dolist (step program)
        (dolist (line (pybricks-step step names))
          (format s "~a~%" line))))))
