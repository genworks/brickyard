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
;;   (drive 200 -30 1500)     drive_base.drive(200, -30), wait(1500), drive_base.stop()
;;   (settings :straight-speed 200 :turn-rate 90)   drive_base.settings(...)
;;   (arm 45 :speed 200)      arm.run_angle(200, 45)         degrees at the arm, deg/s
;;   (wait 1000)              wait(1000)                     milliseconds
;;
;; The simulated robot drives as a Pybricks drive base does: each move
;; follows a trapezoid of speed, the wheels' angles count the distance
;; and the hub's gyroscope the heading, and the settings start at
;; Pybricks' own defaults for the motors it has: 40% of the top speed
;; straight, a third of the top turn rate, three quarters of the motor's
;; acceleration.  Nothing about the table is known to the robot, so a
;; wheel that slips or a wall in the way leaves it where its program does
;; not think it is.
;;

(defparameter *sample-program* "(straight 1560)
(wait 1000)
(straight -300)"
  "String. What the page's program box starts with: two balls along the line, into the red ring.")

(defparameter *motor-acceleration* (* 2000 (/ pi 180))
  "Radians a second a second: what a motor's speed control allows.  An
estimate; Pybricks' own figure is in its servo settings.")

(defparameter *step-names* '(:straight :turn :arc :drive :settings :arm :wait)
  "List of the step names a program may use.")

(defstruct pilot
  "How the robot means to drive: the heading it holds, counterclockwise
radians in the table's frame, and Pybricks' four drive settings in
Pybricks' units, mm/s, mm/s/s, deg/s and deg/s/s."
  (heading 0d0 :type double-float)
  straight-speed straight-acceleration turn-rate turn-acceleration)

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
        (:drive (places 3)
         (checked-options step nil)
         (list :drive (check-number (first places) -1000 1000 "A speed")
               (check-number (second places) -1000 1000 "A turn rate")
               (check-number (third places) 0 150000 "Milliseconds")))
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
        (t (error "~(~s~) is not a step: the steps are ~{~(~a~)~^ ~}." name *step-names*))))))

(defun checked-program (program)
  (unless (and (listp program) (<= (length program) *program-limit*))
    (error "A program is a list of at most ~a steps." *program-limit*))
  (mapcar #'checked-step program))

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

(defun step-text (step)
  "A checked step as a program writes it, without its parentheses."
  (flet ((text (x) (if (realp x) (number-text x) (princ-to-string x))))
    (let* ((places (loop for x in (rest step) until (keywordp x) collect x))
           (keys (nthcdr (length places) (rest step))))
      (format nil "~(~a~)~{ ~a~}~{ ~(~s~) ~a~}" (first step) (mapcar #'text places)
              (loop for (key value) on keys by #'cddr
                    when value append (list key (text value)))))))

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

(defun default-pilot (robot heading)
  "The settings Pybricks starts a drive base on, for this robot's motors and wheels."
  (let* ((top (reduce #'min (robot-wheel-drives robot) :key #'drive-top))
         (half (/ (or (robot-track robot) 100) 2))
         (acceleration (* 0.75d0 *motor-acceleration* (/ top *free-speed*))))
    (make-pilot :heading (float heading 1d0)
                :straight-speed (* 0.4d0 top)
                :straight-acceleration acceleration
                :turn-rate (/ (* (/ top half) (/ 180 pi)) 3)
                :turn-acceleration (* (/ acceleration half) (/ 180 pi)))))

(defun step-controller (step robot pilot)
  "A function of dt that drives the robot through step, answering nil
until it is done, then :done, or :stalled when it never gets there."
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
             (finish (outcome) (stop-sides! robot) outcome))
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
         (destructuring-bind (speed rate milliseconds) (rest step)
           ;; speed and turn rate come up at the settings' accelerations,
           ;; and the robot follows where they would have taken it
           (let ((heading-ref (rigid-angle rigid))
                 (start (rolled robot))
                 (s-ref 0d0) (v 0d0) (w 0d0)
                 (spin (- (radians rate))))
             #'(lambda (dt)
                 (incf elapsed dt)
                 (let ((up (* dt (pilot-straight-acceleration pilot)))
                       (turn-up (* dt (radians (pilot-turn-acceleration pilot)))))
                   (setf v (+ v (within (- speed v) up))
                         w (+ w (within (- spin w) turn-up))))
                 (incf s-ref (* v dt))
                 (incf heading-ref (* w dt))
                 (steer (+ v (* gain (- s-ref (- (rolled robot) start)))) (+ w (* gain (off heading-ref))) dt)
                 (when (>= (* 1000 elapsed) milliseconds)
                   ;; where it got to is the heading from here on
                   (setf (pilot-heading pilot) (rigid-angle rigid))
                   (finish :done))))))
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
               (stop-sides! robot)
               (when (>= (* 1000 elapsed) milliseconds) :done))))
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
  (let* ((pilot (default-pilot robot (place-robot! robot table)))
         (rigid (robot-rigid robot))
         (pieces (table-pieces table))
         (rigids (append (list rigid) pieces (table-walls table)
                         (mapcar #'(lambda (f) (apply #'fixed-rigid f)) (getf table :fixed))))
         (treads (robot-treads robot))
         (seconds 0d0)
         (tick 0)
         (controller nil)
         (left program)
         (current nil)
         (frames nil)
         (ended nil)
         (quiet 0d0))
    (when (touches-of (list* rigid (nthcdr (1+ (length pieces)) rigids)))
      (error "It does not fit on the table's start."))
    (loop
      (when (and (null controller) left)
        (setf current (pop left)
              controller (step-controller current robot pilot)))
      (when controller
        (let ((outcome (funcall controller *tick*)))
          (when outcome
            (push (list current outcome (/ (round (* 10 seconds)) 10d0)) ended)
            (setf controller nil))))
      (dolist (tread treads) (roll-tread! rigid tread *tick*))
      (dolist (arm (robot-arms robot)) (swing-arm! arm robot *tick*))
      (when (robot-arms robot) (pose-arms! robot))
      (feel-touches! robot (step-rigids! rigids *tick*) *tick*)
      (incf seconds *tick*)
      (when (zerop (mod tick *frame-ticks*))
        (push (frame-of seconds robot pieces) frames))
      (incf tick)
      ;; a program done, the table comes to rest
      (when (and (null controller) (null left))
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
