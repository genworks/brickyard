;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The editor: one page, /brickyard?b=<build id>, with an X_ITE canvas,
;; and one POST address, /brickyard/api, that takes an operation on the
;; build and answers in JSON with only the parts that changed.  Every
;; part travels as its own small X3D document: a named Transform holding
;; a PlaneSensor and, inside a second Transform, the x3d lens's own
;; output for the part's leaves.  The PlaneSensor's plane is the
;; model's horizontal (the scene is z-up inside the page's y-up
;; wrapper), so a part dragged in the browser slides across the build,
;; and on release the page sends the move here to be dropped and snapped.
;;
;; Builds live in memory and are written to /projects/.state/
;; brickyard-<id>.sexp after every change, so they survive a restart.
;;

(defparameter *state-directory* #p"/projects/.state/")

(defparameter *static-directory*
  (merge-pathnames "../static/" (make-pathname :name nil :type nil :defaults (glisp::source-pathname)))
  "Pathname. The page's script and style sheet.")

(defvar *builds* (make-hash-table :test #'equal) "Hash table from build id to its assembly.")

(defvar *builds-lock* (bt:make-lock "brickyard builds"))

(defparameter *palette*
  '(("Bricks" (:brick (1 1)) (:brick (1 2)) (:brick (1 4)) (:brick (2 2)) (:brick (2 4)) (:brick (2 6)))
    ("Plates" (:plate (1 2)) (:plate (1 4)) (:plate (2 2)) (:plate (2 4)) (:plate (2 8)) (:plate (4 8)) (:plate (6 10)))
    ("Tiles" (:tile (1 2)) (:tile (2 2)) (:round-tile nil) (:round-plate nil))
    ("Technic" (:technic-brick (4)) (:technic-brick (8)) (:technic-brick (16))
     (:beam (1)) (:beam (2)) (:beam (3)) (:beam (5)) (:beam (7)) (:beam (9)) (:beam (13)) (:beam (15))
     (:l-beam (2 4)) (:l-beam (3 5)) (:t-beam nil)
     (:axle (2)) (:axle (3)) (:axle (4)) (:axle (6)) (:axle (7)) (:axle (8)) (:axle (12))
     (:gear (8)) (:gear (12)) (:gear (16)) (:gear (20)) (:gear (24)) (:gear (40)))
    ("Connectors" (:pin (2)) (:pin (3)) (:half-pin nil) (:stop-pin nil) (:hole-pin nil)
     (:axle-pin (1)) (:axle-pin (2)) (:bush nil)
     (:cross-block (2)) (:cross-block (3)) (:axle-block nil) (:pin-block nil)
     (:angle-connector (1)) (:angle-connector (2)) (:axle-joiner (2)) (:axle-joiner (3)) (:pin-joiner nil))
    ("Robot" (:wheel (3 1)) (:wheel (4 2)) (:wheel (5 2)) (:wheel (7 3)) (:rim nil) (:tyre nil)
     (:motor nil) (:controller nil)))
  "List of (heading (type size) ...) for the page's parts bin.")

(defparameter *colors*
  '(:red :blue :yellow :green :black :white :grey :grey-light :orange :tan :brown :lime))

;;
;; Builds
;;

(defun valid-build-id-p (id)
  (and (stringp id) (<= 1 (length id) 32)
       (every #'(lambda (c) (or (digit-char-p c) (char<= #\a c #\z) (char= c #\-))) id)))

(defun build-file (id)
  (merge-pathnames (format nil "brickyard-~a.sexp" id) *state-directory*))

(defun new-build-id ()
  (let ((*random-state* (make-random-state t)))
    (format nil "~(~36r~)" (+ (expt 36 5) (random (* 35 (expt 36 5)))))))

(defun save-build (id assembly)
  (let ((file (build-file id)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :brickyard)))
          (prin1 (the-object assembly part-specs) out)
          (terpri out))))))

(defun find-build (id)
  "The assembly of build id, read from its file when it is not in memory;
nil when there is none."
  (when (valid-build-id-p id)
    (or (gethash id *builds*)
        (let ((file (build-file id)))
          (when (probe-file file)
            (setf (gethash id *builds*)
                  (make-object 'assembly
                               :part-specs (ensure-ids (with-open-file (in file)
                                                         (with-standard-io-syntax
                                                           (let ((*package* (find-package :brickyard))
                                                                 (*read-eval* nil))
                                                             (read in))))))))))))

(defun make-build (&key (specs *sample-build*))
  (let ((id (new-build-id)))
    (setf (gethash id *builds*) (make-object 'assembly :part-specs (ensure-ids specs)))
    (save-build id (gethash id *builds*))
    id))

;;
;; Parts as X3D, and the build's state as JSON
;;

(defun part-of (assembly id)
  (let ((index (position id (the-object assembly part-specs) :key #'spec-id)))
    (when index (the-object assembly (parts index)))))

(defun part-x3d (assembly id)
  (with-output-to-string (s)
    (format s "<X3D profile=\"Interchange\" version=\"4.0\"><head><component name=\"PointingDeviceSensor\" level=\"1\"/></head><Scene><Transform DEF=\"p~a\"><PlaneSensor DEF=\"p~as\" autoOffset=\"false\"/><Transform DEF=\"p~ad\">"
            id id id)
    (with-format (x3d s)
      (dolist (leaf (the-object (part-of assembly id) leaves))
        (write-the-object leaf cad-output)))
    (format s "</Transform></Transform></Scene></X3D>")))

(defun json-object (&rest keys-and-values)
  (let ((table (make-hash-table :test #'equal)))
    (do ((rest keys-and-values (cddr rest))) ((null rest) table)
      (setf (gethash (string-downcase (first rest)) table) (second rest)))))

(defun part-json (assembly id)
  (let ((spec (find id (the-object assembly part-specs) :key #'spec-id)))
    (json-object :id id :at (coerce (spec-at spec) 'vector)
                 :label (part-label (spec-type spec) (spec-size spec)) :x3d (part-x3d assembly id))))

(defun part-label (type size)
  "String naming a part the way the bin does: Brick 2x4, Technic brick 1x8, Beam 5."
  (case type
    (:pin (if (eql (first size) 3) "Pin 3" "Pin"))
    (:half-pin "Half pin")
    (:axle-pin (if (eql (first size) 2) "Axle-pin with 2L pin" "Axle-pin"))
    (:cross-block (format nil "Cross block 1x~a" (first size)))
    (:l-beam (format nil "L-beam ~{~a~^x~}" size))
    (:t-beam "T-beam 3x3")
    (:round-tile "Round tile 1x1")
    (:round-plate "Round plate 1x1")
    (:angle-connector (format nil "Angle connector #~a" (first size)))
    (:axle-joiner (if (eql (first size) 2) "Axle joiner" "Axle joiner 3L"))
    (:pin-joiner "Pin joiner")
    (:stop-pin "Pin 3 with stop bush")
    (:hole-pin "Pin 3 with pin hole")
    (:pin-block "Cross block 1x3 with 4 pins")
    (:axle-block "Cross block 1x3 (axle/pin/axle)")
    (:rim "Rim 20x30")
    (:tyre "Tyre 48x20")
    (t (format nil "~:(~a~)~@[ ~a~]"
               (substitute #\Space #\- (string type))
               (case type
                 (:brick (destructuring-bind (w l h) size
                           (cond ((= h 3) (format nil "~ax~a" w l))
                                 ((zerop (mod h 3)) (format nil "~ax~ax~a" w l (/ h 3)))
                                 (t (format nil "~ax~a, ~a plates high" w l h)))))
                 ((:plate :tile :technic-brick :wheel) (format nil "~ax~a" (first size) (second size)))
                 ((:beam :axle :gear) (format nil "~a" (first size))))))))

(defun bin-label (type size)
  (case type
    (:pin (if (eql (first size) 3) "Pin 3" "Pin"))
    (:half-pin "Half pin") (:bush "Bush") (:round-tile "Round 1x1") (:round-plate "Round plate")
    (:l-beam (format nil "L ~{~a~^x~}" size)) (:t-beam "T 3x3")
    (:stop-pin "Stop pin") (:hole-pin "Hole pin") (:pin-joiner "Pin joiner")
    (:axle-block "Cross A/P/A") (:pin-block "Cross 4 pins")
    (:angle-connector (format nil "Angle #~a" (first size)))
    (:axle-joiner (if (eql (first size) 2) "Joiner" "Joiner 3L"))
    (:rim "Rim") (:tyre "Tyre")
    (:axle-pin (if (eql (first size) 2) "Axle-pin 2" "Axle-pin"))
    (:cross-block (format nil "Cross 1x~a" (first size)))
    (:motor "Motor") (:controller "Controller")
    (t (format nil "~:[~;~:*~a ~]~{~a~^x~}"
               (case type (:beam "Beam") (:axle "Axle") (:technic-brick "Tech") (:wheel "Wheel") (:gear "Gear"))
               size))))

(defun status-json (assembly)
  (let* ((specs (the-object assembly part-specs))
         (id-of #'(lambda (index) (spec-id (nth index specs)))))
    (json-object
     :count (length specs)
     :joined (length (the-object assembly connections))
     :collisions (coerce (mapcar #'(lambda (pair) (coerce (mapcar id-of pair) 'vector))
                                 (the-object assembly collisions))
                         'vector)
     :loose (coerce (mapcar id-of (the-object assembly loose)) 'vector)
     :drives (coerce (mapcar #'(lambda (drive)
                                 (destructuring-bind (motor wheel speed) drive
                                   (json-object :motor (funcall id-of motor) :wheel (funcall id-of wheel)
                                                :speed (format nil "~a" (abs speed))
                                                :reversed (minusp speed))))
                             (the-object assembly drives))
                     'vector)
     :meshes (count-if #'(lambda (c) (member :mesh (fourth c))) (the-object assembly connections))
     :jammed (coerce (mapcar id-of (the-object assembly jammed)) 'vector)
     :ground (coerce (mapcar id-of (the-object assembly ground-contacts)) 'vector)
     :bom (coerce (mapcar #'(lambda (entry)
                              (destructuring-bind (count type size color) entry
                                (json-object :count count :color (string-downcase color)
                                             :label (part-label type size))))
                          (the-object assembly bill-of-materials))
                  'vector))))

(defun reply (assembly &key changed removed message (ok t))
  (json-object :ok ok :message message
               :parts (coerce (mapcar #'(lambda (id) (part-json assembly id)) changed) 'vector)
               :removed (coerce removed 'vector)
               :status (status-json assembly)))

;;
;; Operations
;;

(defun read-number (string)
  (let ((value (and string (ignore-errors (let ((*read-eval* nil)) (read-from-string string))))))
    (if (realp value) value 0)))

(defun read-keyword (string allowed)
  (find string allowed :test #'string-equal))

(defun read-size (string)
  "List of integers from a string like \"2 4\"; nil for none."
  (let ((*read-eval* nil))
    (ignore-errors
     (let ((list (read-from-string (format nil "(~a)" (or string "")))))
       (when (every #'(lambda (n) (and (integerp n) (<= 1 n 32))) list) list)))))

(defun others-than (specs id) (remove id specs :key #'spec-id))

(defun clear-spot (spec others)
  "The spec moved off to the right of everything, on the ground, where it is clear."
  (let ((x (+ 60 (reduce #'max (mapcar #'(lambda (o) (first (second (world-extent o)))) others)
                         :initial-value 0))))
    (do ((candidate (move-spec spec (list x 0 0)) (move-spec spec (list (incf x 20) 0 0))))
        ((not (collides-with-any-p candidate others)) candidate))))

(defun operate (assembly op params)
  "Do op on the build with params (an alist of strings).  Returns the reply."
  (let* ((specs (the-object assembly part-specs))
         (id (let ((v (cdr (assoc "id" params :test #'string=)))) (and v (read-number v))))
         (spec (and id (find id specs :key #'spec-id)))
         (param #'(lambda (name) (cdr (assoc name params :test #'string=))))
         (store #'(lambda (new-specs) (the-object assembly (set-slot! :part-specs new-specs)))))
    (flet ((replace-spec (new)
             (funcall store (substitute new spec specs :test #'eql))
             (reply assembly :changed (list id)))
           (refuse (message) (reply assembly :ok nil :message message)))
      (cond
        ((string= op "state")
         (reply assembly :changed (mapcar #'spec-id specs)))

        ((string= op "add")
         (let* ((type (read-keyword (funcall param "type") '(:brick :plate :tile :technic-brick :beam :axle :pin
                                                              :wheel :motor :controller :gear :half-pin :axle-pin
                                                              :bush :cross-block :l-beam :round-tile
                                                              :angle-connector :axle-joiner :pin-joiner :stop-pin
                                                              :hole-pin :pin-block :axle-block :t-beam :round-plate
                                                              :rim :tyre)))
                (size (read-size (funcall param "size")))
                (color (read-keyword (funcall param "color") *colors*))
                (near (let ((v (funcall param "near"))) (and v (find (read-number v) specs :key #'spec-id))))
                (new-id (1+ (reduce #'max specs :key #'spec-id :initial-value -1))))
           (if (null type)
               (refuse "Unknown part type.")
               (let* ((new (list :id new-id :type type :size size :color color))
                      (start (if near
                                 (let ((extent (world-extent near)))
                                   (list (round (+ (first (first extent)) (first (second extent))) 2)
                                         (round (+ (second (first extent)) (second (second extent))) 2)
                                         (third (second extent))))
                                 (list 0 0 0)))
                      (placed (or (and specs (fit-spec (move-spec new start) specs))
                                  (if specs (clear-spot new specs) (move-spec new '(0 0 0))))))
                 (funcall store (append specs (list placed)))
                 (reply assembly :changed (list new-id))))))

        ((null spec) (refuse "No such part."))

        ;; swing by hand: degrees about the pin or axle the part (and what
        ;; it carries, unless carry=0) hangs on, or about the vertical
        ((string= op "rotate")
         (let* ((index (position spec specs))
                (survey (survey specs))
                (moving (if (string= (or (funcall param "carry") "1") "0") (list index) (carried specs index survey)))
                (line (pivot-line specs moving survey))
                (degrees (let ((d (read-number (funcall param "degrees")))) (max -180 (min 180 d))))
                (radians (* degrees (/ pi 180)))
                (group (mapcar #'(lambda (i) (nth i specs)) moving))
                (others (remove-if #'(lambda (s) (member s group :test #'eq)) specs)))
           (if (eq line :locked)
               (refuse "It is held on two pins or axles; it cannot swing.")
               (let* ((point (if line (first line) (spec-at spec)))
                      (axis (if line (second line) '(0 0 1)))
                      (swung (mapcar #'(lambda (s) (swing-spec s point axis radians)) group)))
                 (if (group-clear-p swung others)
                     (progn
                       (funcall store (mapcar #'(lambda (s) (let ((k (position s group :test #'eq))) (if k (nth k swung) s))) specs))
                       (reply assembly :changed (mapcar #'spec-id swung)
                                       :message (format nil "Swung ~d degrees about ~a." degrees
                                                        (if line "the pin or axle it hangs on" "the vertical"))))
                     (refuse "No room to swing it there."))))))

        ((string= op "carried")
         (json-object :ok t :carried (coerce (mapcar #'(lambda (i) (spec-id (nth i specs)))
                                                     (carried specs (position spec specs)))
                                             'vector)))

        ((member op '("move" "turn" "tilt" "roll") :test #'string=)
         ;; the part and what it carries, unless carry=0 (shift held)
         (let* ((group (mapcar #'(lambda (i) (nth i specs))
                               (if (string= (or (funcall param "carry") "1") "0")
                                   (list (position spec specs))
                                   (carried specs (position spec specs)))))
                (ids (mapcar #'spec-id group))
                (others (remove-if #'(lambda (s) (member (spec-id s) ids)) specs))
                (about (cdr (assoc op '(("turn" . :z) ("tilt" . :x) ("roll" . :y)) :test #'string=)))
                (dropped
                  (if about
                      (drop-group (turn-group group about) others)
                      (drop-group (shift-specs group (mapcar #'(lambda (k) (round (read-number (funcall param k))))
                                                             '("dx" "dy" "dz")))
                                  others
                                  :reach (if (string= (or (funcall param "exact") "") "1") 0 40)))))
           (if dropped
               (progn
                 (funcall store (mapcar #'(lambda (s) (or (find (spec-id s) dropped :key #'spec-id) s)) specs))
                 (reply assembly :changed ids))
               (reply assembly :ok nil :changed ids
                             :message (if about "No room to turn it there." "No room there.")))))

        ((string= op "color")
         (let ((color (read-keyword (funcall param "color") *colors*)))
           (if color
               (let ((new (copy-list spec))) (setf (getf new :color) color) (replace-spec new))
               (refuse "Unknown color."))))

        ((string= op "copy")
         (let* ((new-id (1+ (reduce #'max specs :key #'spec-id :initial-value -1)))
                (copy (list* :id new-id (let ((rest (copy-list spec))) (remf rest :id) rest)))
                (placed (or (drop-spec (move-spec copy (v+ (spec-at spec) (list 0 0 (* 8 (or (third (spec-size spec)) 3)))))
                                       specs :free? nil)
                            (clear-spot copy specs))))
           (funcall store (append specs (list placed)))
           (reply assembly :changed (list new-id))))

        ((string= op "delete")
         (funcall store (others-than specs id))
         (reply assembly :removed (list id)))

        (t (refuse "Unknown operation."))))))

;;
;; The page
;;

(defun page-html (build-id &optional note)
  (cl-who:with-html-output-to-string (s nil :prologue "<!doctype html>")
    (:html :lang "en"
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "Brickyard")
      (:link :rel "stylesheet" :href "/brickyard/static/brickyard.css")
      (:script :src gwl::*x-ite-url*)
      (:script :src "/brickyard/static/brickyard.js" :defer "defer"))
     ;; cl-who writes attribute values raw: the note is escaped here
     (:body :data-build build-id :data-note (and note (cl-who:escape-string note))
      (:header (:h1 "Brickyard")
       (:span :id "build-name" (cl-who:str build-id))
       (:a :href "/brickyard?new=1" "New build")
       (:a :href "/brickyard?new=robot" "Robot build")
       (:a :href "/brickyard?new=gearbox" "Gearbox")
       (:a :href "/brickyard?new=empty" "Empty build")
       (:a :href (format nil "/brickyard/export?b=~a" build-id) :id "export" "Export LDraw")
       (:label :class "import" "Import LDraw"
        (:input :type "file" :id "import-file" :accept ".ldr,.mpd,.dat,.l3b" :hidden "hidden")))
      (:main
       (:aside :id "bin"
        (dolist (group *palette*)
          (cl-who:htm
           (:h2 (cl-who:str (first group)))
           (:div :class "bin-row"
            (dolist (entry (rest group))
              (destructuring-bind (type size) entry
                (cl-who:htm
                 (:button :class "part" :data-type (string-downcase type)
                          :data-size (format nil "~{~a~^ ~}" size)
                          (cl-who:str (bin-label type size)))))))))
        (:h2 "Color")
        (:div :class "bin-row" :id "swatches"
         (dolist (color *colors*)
           (let ((rgb (coerce (lookup-color color) 'list)))
             (cl-who:htm
              (:button :class "swatch" :data-color (string-downcase color) :title (string-downcase color)
                       :style (format nil "background: rgb(~{~d~^,~})"
                                      (mapcar #'(lambda (c) (round (* 255 c))) rgb))))))))
       (:section :id "stage"
        (:|x3d-canvas| :id "canvas" :|contextMenu| "false" :|splashScreen| "false"
         :|notifications| "false" :|timings| "false")
        (:div :id "message")
        (:script :type "model/x3d+xml" :id "scene" (cl-who:str (scene-x3d))))
       (:aside :id "report"
        (:h2 "Build") (:div :id "summary")
        (:h2 "Parts") (:table :id "bom")
        (:h2 "Keys")
        (:dl
         (:dt "drag") (:dd "slide a part and what it carries; it snaps on release")
         (:dt "arrows") (:dd "nudge a stud; PgUp / PgDn a plate")
         (:dt "R T E") (:dd "turn, tilt, roll a quarter")
         (:dt "[ ]") (:dd "swing 15 degrees about the pin or axle it hangs on (Alt: 5)")
         (:dt "Shift") (:dd "with any of these: the part alone")
         (:dt "C") (:dd "copy on top")
         (:dt "Del") (:dd "remove")
         (:dt "Esc") (:dd "let go"))))))))

(defun scene-x3d ()
  "The page's starting world: z-up model space inside a y-up wrapper, and
an empty group the parts go into."
  (format nil "<X3D profile=\"Interchange\" version=\"4.0\"><head><component name=\"PointingDeviceSensor\" level=\"1\"/></head><Scene><NavigationInfo type='\"EXAMINE\" \"ANY\"' headlight=\"true\"></NavigationInfo><Background skyColor=\"0.94 0.95 0.97\"></Background><Viewpoint DEF=\"eye\" position=\"~a\" orientation=\"~a\" fieldOfView=\"0.7\"></Viewpoint><Transform DEF=\"world\" rotation=\"~a\"><Group DEF=\"parts\"></Group></Transform></Scene></X3D>"
          "180 160 240"
          (geom-base::x3d-rotation-string
           (geom-base::look-at-rotation (make-vector -180 -160 -240) (make-vector 0 1 0)))
          geom-base::*z-up->y-up-rotation-string*))

;;
;; Publishing
;;

(defun respond (req ent content-type body &key (code net.aserve:*response-ok*))
  (net.aserve:with-http-response (req ent :content-type content-type :response code)
    (net.aserve:with-http-body (req ent)
      (write-string body (net.aserve:request-reply-stream req)))))

(defun redirect (req ent location)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
    (setf (net.aserve:reply-header-slot-value req :location) location)
    (net.aserve:with-http-body (req ent))))

(defun page-handler (req ent)
  (let ((new (net.aserve:request-query-value "new" req))
        (id (net.aserve:request-query-value "b" req)))
    (cond (new
           (redirect req ent (format nil "/brickyard?b=~a"
                                     (bt:with-lock-held (*builds-lock*)
                                       (make-build :specs (cond ((string= new "empty") nil)
                                                                ((string= new "robot") *robot-build*)
                                                                ((string= new "gearbox") *gearbox-build*)
                                                                (t *sample-build*)))))))
          ((and id (bt:with-lock-held (*builds-lock*) (find-build id)))
           (respond req ent "text/html; charset=utf-8"
                    (page-html id (let ((note (net.aserve:request-query-value "note" req)))
                                    (and note (subseq note 0 (min 600 (length note))))))))
          (t (redirect req ent "/brickyard?new=1")))))

(defun export-handler (req ent)
  (let* ((id (net.aserve:request-query-value "b" req))
         (text (bt:with-lock-held (*builds-lock*)
                 (let ((assembly (find-build id)))
                   (when assembly (to-ldraw assembly :name (format nil "brickyard-~a" id)))))))
    (if text
        (net.aserve:with-http-response (req ent :content-type "application/x-ldraw")
          (net.aserve:with-http-body (req ent :headers (list (cons :content-disposition
                                                                   (format nil "attachment; filename=\"brickyard-~a.mpd\"" id))))
            (write-string text (net.aserve:request-reply-stream req))))
        (respond req ent "text/plain" "No such build." :code net.aserve:*response-not-found*))))

(defun import-handler (req ent)
  "POST an LDraw file's text; answers {ok, build, note} or {ok: false, message}."
  (let* ((declared (parse-decimal (or (net.aserve:header-slot-value req :content-length) "0")))
         (reply
           (if (or (null declared) (> declared (getf *ldraw-limits* :bytes)))
               (json-object :ok nil :message "That file is larger than Brickyard takes (4 MB).")
               (multiple-value-bind (specs report) (from-ldraw (gwl:request-body-text req))
                 (cond ((getf report :too-big)
                        (json-object :ok nil :message "That file is larger than Brickyard takes (4 MB)."))
                       ((null specs)
                        (json-object :ok nil :message (format nil "Nothing in that file could be placed.~a"
                                                              (import-note 0 report))))
                       (t (json-object :ok t
                                       :build (bt:with-lock-held (*builds-lock*) (make-build :specs specs))
                                       :note (import-note (length specs) report))))))))
    (respond req ent "application/json" (with-output-to-string (s) (yason:encode reply s)))))

(defun api-handler (req ent)
  (let* ((params (net.aserve:form-urlencoded-to-query (gwl:request-body-text req)))
         (id (cdr (assoc "b" params :test #'string=)))
         (op (or (cdr (assoc "op" params :test #'string=)) ""))
         (result (bt:with-lock-held (*builds-lock*)
                   (let ((assembly (find-build id)))
                     (if assembly
                         (let ((reply (operate assembly op params)))
                           (unless (string= op "state") (save-build id assembly))
                           reply)
                         (json-object :ok nil :message "No such build."))))))
    (respond req ent "application/json"
             (with-output-to-string (s) (yason:encode result s)))))

(defun publish-brickyard (server)
  (net.aserve:publish :path "/brickyard" :server server :function #'page-handler)
  (net.aserve:publish :path "/brickyard/api" :server server :function #'api-handler)
  (net.aserve:publish :path "/brickyard/export" :server server :function #'export-handler)
  (net.aserve:publish :path "/brickyard/import" :server server :function #'import-handler)
  (net.aserve:publish-directory :prefix "/brickyard/static/" :server server
                                :destination (namestring *static-directory*)))

;; Published with the rest when the server starts; and now, when it is
;; already running (a load into a live room).
(pushnew 'publish-brickyard gwl::*publishers*)

(when (boundp 'net.aserve:*wserver*)
  (publish-brickyard net.aserve:*wserver*))
