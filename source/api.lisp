;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The API: a package of flat functions, brickyard-api, one for each
;; thing a person does at the page, on the current build.  They take
;; literals (numbers, keywords, t and nil, lists as data) and answer
;; plists and strings, and a refusal is an error saying why, so a Lisp
;; caller, or a model driving them, can read what went wrong and try
;; again.  docs/api.md is their reference.
;;
;;   (brickyard-api:use-build (brickyard-api:new-build))
;;   (brickyard-api:add-part :technic-brick :size '(8) :at '(0 0 0))
;;   (brickyard-api:add-part :brick :size '(1 4) :on 0)
;;   (brickyard-api:describe-build)
;;
;; *operators* is what may be run on behalf of someone else: a form from
;; outside runs only when it names one of these and passes literals.
;; use-build and new-build are not among them; the door chooses the build.
;;

(defvar *api-build* nil "String. The id of the build the API works on.")

(defparameter *api-part-limit* 2000 "Integer. The most parts the API lets a build hold.")

(defparameter *operators*
  '(brickyard-api:parts brickyard-api:report brickyard-api:joints brickyard-api:describe-build
    brickyard-api:add-part brickyard-api:move-part brickyard-api:turn-part brickyard-api:swing-part
    brickyard-api:color-part brickyard-api:copy-part brickyard-api:remove-part brickyard-api:clear-build)
  "List of the symbols a form from outside may call.")

(defparameter *docs-directory*
  (merge-pathnames "../docs/" (make-pathname :name nil :type nil :defaults (glisp::source-pathname)))
  "Pathname. The API's reference.")

(defmacro with-api-build ((assembly) &body body)
  "Run body with assembly bound to the current build, the builds locked."
  `(bt:with-lock-held (*builds-lock*)
     (let ((,assembly (or (find-build *api-build*)
                          (error "There is no current build; call use-build first."))))
       ,@body)))

(defun spec-plist (spec)
  "A part as the API shows it."
  (append (list :id (spec-id spec) :type (spec-type spec))
          (when (spec-size spec) (list :size (spec-size spec)))
          (list :at (spec-at spec))
          (if (getf spec :matrix)
              (list :angled t)
              (list :up (spec-up spec) :rear (spec-rear spec)))
          (list :color (spec-color spec))))

(defun id-spec (assembly id)
  (or (and (integerp id) (find id (the-object assembly part-specs) :key #'spec-id))
      (error "There is no part ~s." id)))

(defun joined-ids (assembly id)
  "List of (other-id kinds) for each part joined to part id."
  (let* ((specs (the-object assembly part-specs))
         (index (position id specs :key #'spec-id)))
    (loop for (a b nil kinds) in (the-object assembly connections)
          when (or (eql a index) (eql b index))
            collect (list (spec-id (nth (if (eql a index) b a) specs)) kinds))))

(defun store-build (assembly specs)
  (the-object assembly (set-slot! :part-specs specs))
  (save-build *api-build* assembly))

(defun checked-point (point what)
  (unless (and (listp point) (= (length point) 3) (every #'realp point)
               (every #'(lambda (x) (< (abs x) 100000)) point))
    (error "~a must be a list of three numbers, not ~s." what point))
  (mapcar #'round point))

(defun checked-spec (id type size color up rear)
  "The spec of a new part, or an error naming what is wrong with it."
  (unless (member type *part-types*)
    (error "~s is not a part type; the reference lists them." type))
  (unless (and (listp size) (every #'(lambda (n) (and (integerp n) (<= 1 n 64))) size))
    (error "A size is a list of whole numbers, not ~s." size))
  (when (and color (not (member color *colors*)))
    (error "~s is not a color: ~{~(~s~)~^ ~}." color *colors*))
  (unless (member (list (or up :top) (or rear :rear)) *orientations* :test #'equal)
    (error "~s and ~s are not an orientation: up and rear are two of :top :bottom :left :right :front :rear, at right angles."
           up rear))
  (let ((spec (append (list :id id :type type)
                      (when size (list :size size))
                      (when color (list :color color))
                      (when up (list :up up))
                      (when rear (list :rear rear)))))
    ;; a size the type cannot have fails here, not later in the survey
    (unless (and (<= (length size) (length (spec-size spec)))
                 (ignore-errors (world-extent spec)))
      (error "~s cannot have the size ~s." type size))
    spec))

(defun top-center (spec)
  (let ((extent (world-extent spec)))
    (list (round (+ (first (first extent)) (first (second extent))) 2)
          (round (+ (second (first extent)) (second (second extent))) 2)
          (third (second extent)))))

(defun colliders (spec others)
  (mapcar #'spec-id (remove-if-not #'(lambda (o) (collide-p spec o)) others)))

(defun brickyard-api:use-build (id)
  "Make build id the current build; answers id."
  (unless (bt:with-lock-held (*builds-lock*) (find-build id))
    (error "There is no build ~s." id))
  (setf *api-build* id))

(defun brickyard-api:new-build (&optional specs)
  "A new build of specs (empty by default); answers its id.  It opens at /brickyard?b=<id>."
  (bt:with-lock-held (*builds-lock*) (make-build :specs specs)))

(defun brickyard-api:parts ()
  "List of every part, as plists."
  (with-api-build (assembly)
    (mapcar #'spec-plist (the-object assembly part-specs))))

(defun build-report (assembly)
  (let* ((specs (the-object assembly part-specs))
         (id-of #'(lambda (index) (spec-id (nth index specs)))))
    (list :parts (length specs)
          :joined (length (the-object assembly connections))
          :collisions (mapcar #'(lambda (pair) (mapcar id-of pair)) (the-object assembly collisions))
          :loose (mapcar id-of (the-object assembly loose))
          :meshes (count-if #'(lambda (c) (member :mesh (fourth c))) (the-object assembly connections))
          :drives (mapcar #'(lambda (drive)
                              (destructuring-bind (motor wheel speed) drive
                                (list :motor (funcall id-of motor) :wheel (funcall id-of wheel) :speed speed)))
                          (the-object assembly drives))
          :jammed (mapcar id-of (the-object assembly jammed)))))

(defun brickyard-api:report ()
  "What the build comes to: parts, joints, overlaps, loose parts, what the motors turn."
  (with-api-build (assembly) (build-report assembly)))

(defun brickyard-api:joints (id)
  "List of (other-id kinds): the parts joined to part id, and how."
  (with-api-build (assembly)
    (id-spec assembly id)
    (joined-ids assembly id)))

(defun describe-assembly (assembly &key (limit 300))
  "String. The build in plain words, one line to a part, for a reader who cannot see it."
  (let* ((specs (the-object assembly part-specs))
         (report (build-report assembly)))
    (with-output-to-string (s)
      (if (null specs)
          (format s "The build is empty.~%")
          (progn
            (format s "~a parts, ~a joints (~a gear meshes).~%"
                    (getf report :parts) (getf report :joined) (getf report :meshes))
            (format s "Overlapping: ~:[none~;~:*~s~].  Loose: ~:[none~;~:*~s~].  Jammed: ~:[none~;~:*~s~].~%"
                    (getf report :collisions) (getf report :loose) (getf report :jammed))
            (dolist (drive (getf report :drives))
              (format s "Motor ~a turns wheel ~a at ~a of its speed.~%"
                      (getf drive :motor) (getf drive :wheel) (getf drive :speed)))
            (dolist (spec (subseq specs 0 (min limit (length specs))))
              (format s "~a: ~a~@[ ~(~a~)~] at ~s, ~:[up ~(~a~), rear ~(~a~)~;at an angle~]~%"
                      (spec-id spec) (part-label (spec-type spec) (spec-size spec))
                      (spec-color spec) (spec-at spec)
                      (getf spec :matrix) (spec-up spec) (spec-rear spec)))
            (when (> (length specs) limit)
              (format s "... and ~a more.~%" (- (length specs) limit))))))))

(defun brickyard-api:describe-build ()
  "The build in plain words: what it comes to, then one line to a part."
  (with-api-build (assembly) (describe-assembly assembly)))

(defun brickyard-api:add-part (type &key size color on near at up rear)
  "Add a part, answering it as a plist with :joined.  With :on, joined to
part on, anywhere along it, at the joint nearest the point :near when
given; it turns to fit unless :up or :rear says how it stands.  With
:at, exactly there, refused if it overlaps anything.  With neither, off
to the side of the build."
  (with-api-build (assembly)
    (let* ((specs (the-object assembly part-specs))
           (id (1+ (reduce #'max specs :key #'spec-id :initial-value -1)))
           (new (checked-spec id type size color up rear)))
      (when (>= (length specs) *api-part-limit*)
        (error "A build holds at most ~a parts." *api-part-limit*))
      (let ((placed
              (cond (on
                     (let* ((target (id-spec assembly on))
                            (start (move-spec new (if near (checked-point near ":near") (top-center target)))))
                       (or (if (or up rear)
                               (drop-spec start specs :onto target :reach 10000 :free? nil)
                               (fit-spec start specs :onto target :reach 10000))
                           (error "A ~a will not fit on part ~a: no joint between them is clear."
                                  (part-label type (spec-size new)) on))))
                    (at
                     (let* ((moved (move-spec new (checked-point at ":at")))
                            (hit (colliders moved specs)))
                       (when hit (error "At ~s it overlaps part~p ~{~a~^, ~}." at (length hit) hit))
                       moved))
                    (specs (clear-spot new specs))
                    (t (move-spec new '(0 0 0))))))
        (store-build assembly (append specs (list placed)))
        (append (spec-plist placed) (list :joined (joined-ids assembly id)))))))

(defun api-operate (op id &rest params)
  "Do the editor's own op on part id; answers the part as a plist with
:changed, the ids that moved with it.  A refusal is an error."
  (unless (integerp id) (error "A part id is a whole number, not ~s." id))
  (with-api-build (assembly)
    (let ((reply (operate assembly op (acons "id" (format nil "~d" id) params))))
      (unless (gethash "ok" reply) (error "~a" (or (gethash "message" reply) "Refused.")))
      (save-build *api-build* assembly)
      (let ((spec (find id (the-object assembly part-specs) :key #'spec-id)))
        (append (and spec (spec-plist spec))
                (list :joined (and spec (joined-ids assembly id))
                      :changed (map 'list #'(lambda (part) (gethash "id" part)) (gethash "parts" reply))))))))

(defun carry-param (alone) (cons "carry" (if alone "0" "1")))

(defun brickyard-api:move-part (id dx dy dz &key alone)
  "Move part id and what it carries by dx dy dz LDU, or the part alone."
  (destructuring-bind (dx dy dz) (checked-point (list dx dy dz) "A move")
    (api-operate "move" id (cons "dx" (format nil "~d" dx)) (cons "dy" (format nil "~d" dy))
                 (cons "dz" (format nil "~d" dz)) (cons "exact" "1") (carry-param alone))))

(defun brickyard-api:turn-part (id &key (about :z) alone)
  "Turn part id and what it carries a quarter about the world axis :x, :y or :z."
  (let ((op (cdr (assoc about '((:z . "turn") (:x . "tilt") (:y . "roll"))))))
    (unless op (error "Turn about :x, :y or :z, not ~s." about))
    (api-operate op id (carry-param alone))))

(defun brickyard-api:swing-part (id degrees &key alone)
  "Swing part id and what it carries by degrees about the pin or axle it hangs on."
  (unless (realp degrees) (error "Degrees are a number, not ~s." degrees))
  (api-operate "rotate" id (cons "degrees" (format nil "~d" (round degrees))) (carry-param alone)))

(defun brickyard-api:color-part (id color)
  "Recolor part id."
  (unless (member color *colors*) (error "~s is not a color: ~{~(~s~)~^ ~}." color *colors*))
  (api-operate "color" id (cons "color" (string-downcase color))))

(defun brickyard-api:copy-part (id)
  "A copy of part id on top of it, or off to the side; answers the copy."
  (unless (integerp id) (error "A part id is a whole number, not ~s." id))
  (with-api-build (assembly)
    (id-spec assembly id)
    (let* ((reply (operate assembly "copy" (acons "id" (format nil "~d" id) nil)))
           (new (gethash "id" (aref (gethash "parts" reply) 0))))
      (save-build *api-build* assembly)
      (append (spec-plist (id-spec assembly new)) (list :joined (joined-ids assembly new))))))

(defun brickyard-api:remove-part (id)
  "Remove part id."
  (with-api-build (assembly)
    (id-spec assembly id)
    (store-build assembly (others-than (the-object assembly part-specs) id))
    (list :removed id)))

(defun brickyard-api:clear-build ()
  "Remove every part."
  (with-api-build (assembly)
    (store-build assembly nil)
    (list :parts 0)))

(defun catalog-text ()
  "String. Every part in the bin, as the arguments add-part takes, and the colors."
  (with-output-to-string (s)
    (dolist (group *palette*)
      (format s "~a:~{ ~a~^,~}~%" (first group)
              (mapcar #'(lambda (entry)
                          (destructuring-bind (type size) entry
                            (format nil "~(~s~)~@[ :size ~s~]" type size)))
                      (rest group))))
    (format s "Colors:~{ ~(~s~)~}~%" *colors*)))

(defun reference-text ()
  "String. The API's reference: docs/api.md and the bin as it stands."
  (format nil "~a~%## The parts bin~%~%~a"
          (uiop:read-file-string (merge-pathnames "api.md" *docs-directory*))
          (catalog-text)))
