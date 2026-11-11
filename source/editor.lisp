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

(defvar *agents* nil
  "List of (name sexp-agent turn): the models the prompt box offers, the
first the one it starts on, turn the function of one thunk its prompts
run inside.  Empty until prompting opens the box (api.lisp).")

(defparameter *palette*
  '(("Bricks" (:brick (1 1)) (:brick (1 2)) (:brick (1 4)) (:brick (2 2)) (:brick (2 4)) (:brick (2 6)))
    ("Plates" (:plate (1 2)) (:plate (1 4)) (:plate (2 2)) (:plate (2 4)) (:plate (2 8)) (:plate (4 8)) (:plate (6 10)))
    ("Tiles" (:tile (1 2)) (:tile (2 2)) (:round-tile nil) (:round-plate nil) (:cheese-slope nil))
    ("Technic" (:technic-brick (4)) (:technic-brick (8)) (:technic-brick (16))
     (:beam (1)) (:beam (2)) (:beam (3)) (:beam (5)) (:beam (7)) (:beam (9)) (:beam (13)) (:beam (15))
     (:l-beam (2 4)) (:l-beam (3 5)) (:t-beam nil) (:bent-beam nil) (:h-beam nil) (:axle-beam nil)
     (:axle (2)) (:axle (3)) (:axle (4)) (:axle (6)) (:axle (7)) (:axle (8)) (:axle (12))
     (:stop-axle (4)) (:stop-axle (8)) (:stud-axle nil) (:hole-axle (2)) (:hole-axle (3))
     (:gear (8)) (:gear (12)) (:gear (16)) (:gear (20)) (:gear (24)) (:gear (40)))
    ("Connectors" (:pin (2)) (:pin (3)) (:half-pin nil) (:stop-pin nil) (:hole-pin nil)
     (:axle-pin (1)) (:axle-pin (2)) (:bush nil)
     (:cross-block (2)) (:cross-block (3)) (:axle-block nil) (:pin-block nil)
     (:split-cross-block nil) (:triple-cross-block nil) (:twin-cross-block nil)
     (:angle-connector (1)) (:angle-connector (2)) (:angle-connector (3)) (:angle-connector (5))
     (:axle-joiner (2)) (:axle-joiner (3)) (:pin-joiner nil)
     (:axle-hub nil) (:axle-connector nil) (:cross-pin nil) (:pin-axle nil))
    ("Robot" (:wheel (3 1)) (:wheel (4 2)) (:wheel (5 2)) (:wheel (7 3)) (:rim nil) (:tyre nil)
     (:motor nil) (:controller nil))
    ("Sensors" (:color-sensor nil) (:distance-sensor nil) (:force-sensor nil))
    ("SPIKE Prime" (:spike-hub nil) (:spike-large-motor nil) (:spike-medium-motor nil)
     (:spike-color-sensor nil) (:spike-distance-sensor nil) (:spike-force-sensor nil) (:spike-wheel nil)))
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
    (:bent-beam "Beam 4x6 bent")
    (:cheese-slope "Slope 1x1x2/3")
    (:split-cross-block "Cross block 2x2x2 split")
    (:triple-cross-block "Cross block 3x2 (axle/3 pins)")
    (:twin-cross-block "Cross block 2x2 (axle/2 pins)")
    (:axle-hub "Hub with 2 axles at 90°")
    (:hole-axle (format nil "Axle ~aL with pin hole" (first size)))
    (:stud-axle "Axle 3 with stud")
    (:stop-axle (format nil "Axle ~a with stop" (first size)))
    (:axle-connector "Axle/bush connector")
    (:axle-beam "Beam 2 with axle hole")
    (:h-beam "H-beam 5x3")
    (:cross-pin "Pin with pin hole")
    (:pin-axle "Axle 2 with pin")
    (:spike-hub "SPIKE Prime hub") (:spike-large-motor "SPIKE large motor")
    (:spike-medium-motor "SPIKE medium motor") (:spike-color-sensor "SPIKE color sensor")
    (:spike-distance-sensor "SPIKE distance sensor") (:spike-force-sensor "SPIKE force sensor")
    (:spike-wheel "SPIKE wheel 56")
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
    (:bent-beam "Bent 4x6") (:cheese-slope "Cheese") (:split-cross-block "Cross 2x2x2")
    (:triple-cross-block "Cross 3x2") (:twin-cross-block "Cross 2x2") (:axle-hub "Hub 2 axles")
    (:hole-axle (format nil "Hole axle ~a" (first size))) (:stud-axle "Stud axle")
    (:stop-axle (format nil "Stop axle ~a" (first size))) (:axle-connector "Axle/bush")
    (:axle-beam "Beam 2 axle") (:h-beam "H 5x3") (:cross-pin "Hole pin T") (:pin-axle "Pin-axle 2")
    (:axle-pin (if (eql (first size) 2) "Axle-pin 2" "Axle-pin"))
    (:cross-block (format nil "Cross 1x~a" (first size)))
    (:motor "Motor") (:controller "Controller")
    (:color-sensor "Color sensor") (:distance-sensor "Distance sensor") (:force-sensor "Force sensor")
    (:spike-hub "Hub") (:spike-large-motor "Large motor") (:spike-medium-motor "Medium motor")
    (:spike-color-sensor "Color") (:spike-distance-sensor "Distance") (:spike-force-sensor "Force")
    (:spike-wheel "Wheel 56")
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
     :mesh-pairs (coerce (mapcan #'(lambda (c)
                                     (when (member :mesh (fourth c))
                                       (list (vector (funcall id-of (first c)) (funcall id-of (second c))))))
                                 (the-object assembly connections))
                         'vector)
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
         (let* ((type (read-keyword (funcall param "type") *part-types*))
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
                      ;; anywhere on the selected part first: the free end
                      ;; of a long axle is far from its middle
                      (placed (or (and near (fit-onto new specs near start))
                                  (and specs (fit-spec (move-spec new start) specs))
                                  (if specs (clear-spot new specs) (move-spec new '(0 0 0))))))
                 (funcall store (append specs (list placed)))
                 (reply assembly :changed (list new-id))))))

        ;; what turns, for the page to run: the motors' trains, or with
        ;; no motor turning anything the selected part's, turned by hand
        ((string= op "motion")
         (multiple-value-bind (turning jammed)
             (motion specs :survey (the-object assembly survey) :driver (and spec (position spec specs)))
           (json-object
            :ok (and turning t)
            :message (cond (jammed "The train is jammed: two paths would turn a part at different speeds.")
                           ((null turning) "Nothing turns. Add a motor, or select a gear, axle or wheel to turn it by hand."))
            :motion (coerce (mapcar #'(lambda (entry)
                                        (destructuring-bind (index speed point axis) entry
                                          (json-object :id (spec-id (nth index specs)) :speed (float speed 1d0)
                                                       :point (map 'vector #'(lambda (x) (float x 1d0)) point)
                                                       :axis (map 'vector #'(lambda (x) (float x 1d0)) axis))))
                                    turning)
                            'vector))))

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
                (shifted (unless about
                           (shift-specs group (mapcar #'(lambda (k) (round (read-number (funcall param k))))
                                                      '("dx" "dy" "dz")))))
                (dropped
                  (cond (about (drop-group (turn-group group about) others))
                        ((string= (or (funcall param "nudge") "") "1") (nudge-group shifted others))
                        ((string= (or (funcall param "exact") "") "1") (drop-group shifted others :reach 0))
                        (t (drag-group shifted others)))))
           (if dropped
               (progn
                 (funcall store (mapcar #'(lambda (s) (or (find (spec-id s) dropped :key #'spec-id) s)) specs))
                 (reply assembly :changed ids))
               (let ((hit (and shifted (in-the-way shifted others))))
                 (reply assembly :ok nil :changed ids
                               :message (cond (about "No room to turn it there.")
                                              (hit (format nil "No room there: it would overlap ~a (part ~a)."
                                                           (part-label (spec-type hit) (spec-size hit)) (spec-id hit)))
                                              (t "No room there.")))))))

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
      (:link :rel "stylesheet" :href (static-url "brickyard.css"))
      (:script :src gwl::*x-ite-url*)
      (:script :src (static-url "brickyard.js") :defer "defer"))
     ;; cl-who writes attribute values raw: the note is escaped here
     (:body :data-build build-id :data-note (and note (cl-who:escape-string note))
      (:header (:h1 "Brickyard")
       (:span :id "build-name" :title "This build saves itself as you go; this page's address opens it again."
        (cl-who:str build-id) (:span :class "saved" " saved"))
       (:select :id "your-builds" :title "Builds you have opened in this browser" :hidden "hidden"
        (:option :value "" "Your builds"))
       (:a :href "#" :id "welcome-open" "Start here")
       (:a :href "/brickyard?new=1" "New build")
       (:a :href "/brickyard?new=robot" "Robot build")
       (:a :href "/brickyard?new=arm" "Arm robot")
       (:a :href "/brickyard?new=sensors" "Sensor robot")
       (:a :href "/brickyard?new=spike" "SPIKE robot")
       (:a :href "/brickyard?new=gearbox" "Gearbox")
       (:a :href "/brickyard?new=frame" "Gear frame")
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
        (:section :id "welcome" :hidden "hidden"
         (:h2 "Start here")
         (:ol
          (:li (:b "See one work.") " Open the Gearbox, a motor turning a gear train, and press " (:b "Run") ".")
          (:li (:b "Build a gear train.") " Choose " (:b "Gear frame") ".  Click the first axle so it glows, then " (:b "Gear 8")
               " in the parts bin; the next axle, then " (:b "Gear 24") "; and so on.  Press " (:b "Run") ".")
          (:li (:b "Change it.") " Click or tap a part to select it, and drag it to move it; the buttons beside it turn, copy or delete it.  "
               (:b "Undo") " takes back any change.")
          (:li (:b "Come back to it.") " Your build saves itself as you go.  " (:b "Your builds")
               ", at the top, opens it again, and so does your invite link."))
         (:div :class "welcome-buttons"
          (:a :class "button" :href "/brickyard?new=gearbox" "Open the Gearbox")
          (:a :class "button" :href "/brickyard?new=frame" "Gear frame")
          (:button :type "button" :id "welcome-close" "Got it")))
        (:div :id "part-tools" :hidden "hidden" :aria-label "The selected part"
         (:button :type "button" :data-act "turn" :title "Turn a quarter about the vertical (R)" "Turn")
         (:button :type "button" :data-act "tilt" :title "Tilt a quarter (T)" "Tilt")
         (:button :type "button" :data-act "roll" :title "Roll a quarter (E)" "Roll")
         (:button :type "button" :data-act "rotate" :data-degrees "-15"
                  :title "Swing 15 degrees back about the pin or axle it hangs on ([)" "Swing &#8634;")
         (:button :type "button" :data-act "rotate" :data-degrees "15"
                  :title "Swing 15 degrees on about the pin or axle it hangs on (])" "Swing &#8635;")
         (:button :type "button" :data-act "copy" :title "Copy on top (C)" "Copy")
         (:button :type "button" :data-act "delete" :class "danger" :title "Remove (Del)" "Delete")
         (:button :type "button" :data-act "let-go" :title "Let go (Esc)" "Let go"))
        (:div :id "undo-row"
         (:button :type "button" :id "undo" :title "Undo (Ctrl+Z)" :disabled "disabled" "Undo")
         (:button :type "button" :id "redo" :title "Redo (Ctrl+Y)" :disabled "disabled" "Redo"))
        (:button :type "button" :id "run" "Run")
        (:button :type "button" :id "table-toggle" "Table")
        (:form :id "table-panel" :hidden "hidden"
         (:textarea :id "program" :rows "6" :spellcheck "false" (cl-who:str *sample-program*))
         (:button :type "submit" :id "program-go" "Run program")
         (:button :type "button" :id "program-python" "Pybricks")
         (:pre :id "program-log"))
        (when *agents*
          (cl-who:htm
           (:form :id "prompt"
            (:div :class "prompt-row"
             (:textarea :id "prompt-text" :rows "1" :maxlength "2000"
                        :placeholder "Describe what to build: a small car with four wheels" "")
             (when (rest *agents*)
               (cl-who:htm
                (:select :id "prompt-model" :title "The model that builds"
                 (dolist (entry *agents*)
                   (let ((name (cl-who:escape-string (first entry))))
                     (cl-who:htm (:option :value name (cl-who:str name))))))))
             (:button :type "submit" :id "prompt-go" "Build")
             (:button :type "button" :id "prompt-undo" :hidden "hidden" "Undo"))
            (:div :id "prompt-reply" :hidden "hidden"))))
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
         (:dt "Run") (:dd "turn what the motors drive; with no motor, what the selected gear, axle or wheel turns")
         (:dt "Table") (:dd "stand the build on a table and run a program in Pybricks' terms: (straight 300) (turn 90) (arc 200 :angle 45) (arm -25) (wait 1000); Pybricks writes it out for a real hub")
         (:dt "Shift") (:dd "with any of these: the part alone")
         (:dt "C") (:dd "copy on top")
         (:dt "Del") (:dd "remove")
         (:dt "Esc") (:dd "let go")
         (:dt "Ctrl Z, Y") (:dd "undo, redo"))))))))

(defun scene-x3d ()
  "The page's starting world: z-up model space inside a y-up wrapper, and
an empty group the parts go into."
  (format nil "<X3D profile=\"Interchange\" version=\"4.0\"><head><component name=\"PointingDeviceSensor\" level=\"1\"/></head><Scene><NavigationInfo type='\"EXAMINE\" \"ANY\"' headlight=\"true\" avatarSize=\"8 1.6 0.75\"></NavigationInfo><Background skyColor=\"0.94 0.95 0.97\"></Background><Viewpoint DEF=\"eye\" position=\"~a\" orientation=\"~a\" fieldOfView=\"0.7\"></Viewpoint><Viewpoint DEF=\"table-eye\" position=\"~a\" orientation=\"~a\" centerOfRotation=\"~a\" fieldOfView=\"0.7\"></Viewpoint><Transform DEF=\"world\" rotation=\"~a\"><Transform DEF=\"parts\"></Transform></Transform></Scene></X3D>"
          "180 160 240"
          (geom-base::x3d-rotation-string
           (geom-base::look-at-rotation (make-vector -180 -160 -240) (make-vector 0 1 0)))
          ;; above the table and back from its long side, in y-up terms
          (format nil "~a 2600 1800" (round (getf *table* :width) 2))
          (geom-base::x3d-rotation-string
           (geom-base::look-at-rotation (make-vector 0 -2600 (- -1800 (/ (getf *table* :depth) 2)))
                                        (make-vector 0 1 0)))
          (format nil "~a 0 ~a" (round (getf *table* :width) 2) (- (round (getf *table* :depth) 2)))
          geom-base::*z-up->y-up-rotation-string*))

;;
;; Publishing
;;

(defun respond (req ent content-type body &key (code net.aserve:*response-ok*) last-build)
  "Answer body; with last-build, a build id, the browser remembers it as
the build to reopen (last-build-id)."
  (net.aserve:with-http-response (req ent :content-type content-type :response code)
    (when last-build
      (net.aserve:set-cookie-header req :name "brickyard_last" :value last-build :path "/brickyard"
                                        :expires (+ (get-universal-time) (* 180 86400)) :http-only t))
    (net.aserve:with-http-body (req ent)
      (write-string body (net.aserve:request-reply-stream req)))))

(defun redirect (req ent location)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
    (setf (net.aserve:reply-header-slot-value req :location) location)
    (net.aserve:with-http-body (req ent))))

(defun last-build-id (req)
  "The build this browser last opened, when it is still there: so the
invite link, or /brickyard with no build named, brings a visitor back to
their own work rather than a new sample."
  (let ((id (cdr (assoc "brickyard_last" (net.aserve:get-cookie-values req) :test #'string=))))
    (and (valid-build-id-p id) (bt:with-lock-held (*builds-lock*) (find-build id)) id)))

(defun page-handler (req ent)
  (let ((new (net.aserve:request-query-value "new" req))
        (id (net.aserve:request-query-value "b" req)))
    (cond (new
           (redirect req ent (format nil "/brickyard?b=~a"
                                     (bt:with-lock-held (*builds-lock*)
                                       (make-build :specs (cond ((string= new "empty") nil)
                                                                ((string= new "robot") *robot-build*)
                                                                ((string= new "arm") *arm-build*)
                                                                ((string= new "sensors") *sensor-build*)
                                                                ((string= new "spike") *spike-build*)
                                                                ((string= new "gearbox") *gearbox-build*)
                                                                ((string= new "frame") *gear-frame-build*)
                                                                (t *sample-build*)))))))
          ((and id (bt:with-lock-held (*builds-lock*) (find-build id)))
           (respond req ent "text/html; charset=utf-8"
                    (page-html id (let ((note (net.aserve:request-query-value "note" req)))
                                    (and note (subseq note 0 (min 600 (length note))))))
                    :last-build id))
          (t (let ((last (last-build-id req)))
               (redirect req ent (if last (format nil "/brickyard?b=~a" last) "/brickyard?new=1")))))))

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

(defvar *undo* (make-hash-table :test #'equal)
  "Hash table from build id to its part-specs before each change made
from the page or a prompt, newest first: what undo goes back to.  In
memory: a restart keeps the build, not its history.")

(defvar *redo* (make-hash-table :test #'equal)
  "Hash table from build id to what undo took back, newest first.")

(defparameter *undo-depth* 100 "Integer. The most changes of a build undo can take back.")

(defun note-change! (id before)
  "Before, the build's specs as they were, becomes the newest undo step;
redo is forgotten, as in any editor.  Call with *builds-lock* held."
  (setf (gethash id *undo*) (let ((steps (cons before (gethash id *undo*))))
                              (if (> (length steps) *undo-depth*) (subseq steps 0 *undo-depth*) steps)))
  (remhash id *redo*))

(defun undo-counts (reply id)
  "Reply, telling the page how many steps undo and redo hold for build id."
  (setf (gethash "undo" reply) (length (gethash id *undo*))
        (gethash "redo" reply) (length (gethash id *redo*)))
  reply)

(defun step-back (assembly id from to)
  "Undo (from *undo*, to *redo*) or redo (the other way): the build goes
back to the newest step of from, and its specs as they stand become the
newest of to.  The reply redraws every part."
  (let ((steps (gethash id from)))
    (if (null steps)
        (reply assembly :ok nil :message (if (eq from *undo*) "Nothing to undo." "Nothing to redo."))
        (let ((now (the-object assembly part-specs)))
          (push now (gethash id to))
          (setf (gethash id from) (rest steps))
          (the-object assembly (set-slot! :part-specs (first steps)))
          (reply assembly :removed (mapcar #'spec-id now)
                          :changed (mapcar #'spec-id (the-object assembly part-specs)))))))

(defun api-handler (req ent)
  (let* ((params (net.aserve:form-urlencoded-to-query (gwl:request-body-text req)))
         (id (cdr (assoc "b" params :test #'string=)))
         (op (or (cdr (assoc "op" params :test #'string=)) ""))
         (result (bt:with-lock-held (*builds-lock*)
                   (let ((assembly (find-build id)))
                     (if assembly
                         (let* ((before (the-object assembly part-specs))
                                (reply (cond ((string= op "undo") (step-back assembly id *undo* *redo*))
                                             ((string= op "redo") (step-back assembly id *redo* *undo*))
                                             (t (operate assembly op params)))))
                           (unless (or (eq before (the-object assembly part-specs))
                                       (member op '("undo" "redo") :test #'string=))
                             (note-change! id before))
                           (unless (string= op "state") (save-build id assembly))
                           (undo-counts reply id))
                         (json-object :ok nil :message "No such build."))))))
    (respond req ent "application/json"
             (with-output-to-string (s) (yason:encode result s)))))

;;
;; A private beta's door: when Gendl's invite-gate is loaded, every page
;; and call under /brickyard answers only a request carrying a live
;; invite, minted on the server and handed out as a link.  Without an
;; invites file the gate is open, and without invite-gate there is no
;; gate at all, so development rooms and cold loads run as before.  The
;; static files (the page's own script and style) stay open.
;;
;;   (brickyard:private-beta "/path/to/brickyard-invites.sexp" "https://example.com")
;;   (brickyard:mint-invite! "Name")  ; => token, link
;;   (brickyard:invites-report)
;;   (brickyard:revoke-invite! "token")
;;

(defvar *gate* nil "The invite gate, made on first use when invite-gate is loaded.")

(defun gate ()
  (or *gate*
      (when (find-package :invite-gate)
        (setf *gate* (uiop:symbol-call :invite-gate :make-gate
                                       :name "Brickyard" :prefix "/brickyard" :cookie "brickyard_invite")))))

;; A kept-alive reply first reads the whole request body, as one string
;; of four bytes a character on SBCL, outside any handler: a large POST
;; exhausts the heap and leaves its worker in the debugger for good, and
;; it needs no invite to do it.  So every door here looks at what a
;; request declares before anything reads it, and a refusal closes the
;; connection instead of keeping it alive.  These are here as well as in
;; invite-gate because the images carry a compiled invite-gate until they
;; are rebuilt.

(defparameter *body-limit* 65536
  "Integer. The most bytes a door takes unless it says otherwise: an operation or a prompt is small.")

(defparameter *drained-body-bytes* 1000000
  "Integer. How much of a refused body is read and dropped, so that an
ordinary POST gets its answer.  A larger one is closed on.")

(defparameter *response-payload-too-large* (net.aserve::make-resp 413 "Payload Too Large"))

(defun declared-length (req)
  "The length req declares, or nil for none or one that cannot be read."
  (let ((declared (net.aserve:header-slot-value req :content-length)))
    (and declared (ignore-errors (parse-integer declared)))))

(defun body-within-p (req limit)
  "True when req's body, if it has one, declares a length within limit."
  (let ((declared (net.aserve:header-slot-value req :content-length)))
    (cond ((net.aserve:header-slot-value req :transfer-encoding) nil)
          ((null declared) t)
          (t (let ((length (declared-length req))) (and length (<= 0 length limit)))))))

(defun bodiless-get-p (req)
  (and (member (net.aserve:request-method req) '(:get :head))
       (body-within-p req 0)))

(defun close-after-reply! (req)
  "Called inside with-http-response: the connection closes after this
reply, so the request's body is not read first."
  (setf (net.aserve::request-reply-strategy req)
        (remove :keep-alive (net.aserve::request-reply-strategy req))))

(defun drain-body! (req)
  "Read and drop req's body when nothing has read it and it declares at
most *drained-body-bytes*.  Closing with bytes unread resets the
connection, and the reset can reach the client ahead of the reply."
  (let ((declared (declared-length req)))
    (when (and declared (<= 0 declared *drained-body-bytes*)
               (null (net.aserve::request-request-body req)))
      (handler-case
          (let ((scratch (make-array 65536 :element-type '(unsigned-byte 8)))
                (socket (net.aserve:request-socket req))
                (left declared))
            (setf (net.aserve::request-request-body req) "")
            (when (net.aserve::request-has-continue-expectation req)
              (net.aserve::send-100-continue req))
            (loop while (plusp left)
                  do (let ((got (glisp:with-deadline (10)
                                  (read-sequence scratch socket :end (min left (length scratch))))))
                       (when (zerop got) (return))
                       (decf left got))))
        (serious-condition () nil)))))

(defun refuse-closing (req ent response content-type text)
  "Answer text with response, the request's body unread beyond what is drained."
  (drain-body! req)
  (net.aserve:with-http-response (req ent :content-type content-type :response response)
    (close-after-reply! req)
    (net.aserve:with-http-body (req ent)
      (write-string text (net.aserve:request-reply-stream req)))))

(defun refuse-visitor (req ent)
  "The gate's private-beta page, a 403."
  (refuse-closing req ent net.aserve:*response-forbidden* "text/html"
                  (uiop:symbol-call :invite-gate :refusal-page (gate))))

(defun refuse-body (req ent limit)
  (refuse-closing req ent *response-payload-too-large* "application/json"
                  (with-output-to-string (s)
                    (yason:encode (json-object :ok nil :message (format nil "That is more than Brickyard takes here (~:d bytes)." limit))
                                  s))))

(defun gated (handler &key (limit *body-limit*) (note? t))
  "A publish function that answers through handler only a body within
limit, and only what the gate admits.  The size comes first: asking
the gate looks for ?invite=, and looking reads a form-encoded body.
With note?, what was answered goes in the activity log (admin.lisp)."
  #'(lambda (req ent)
      (let ((gate (gate)))
        (cond ((not (body-within-p req limit)) (refuse-body req ent limit))
              ((and gate (not (uiop:symbol-call :invite-gate :invite-allowed? gate req)))
               (refuse-visitor req ent))
              (t (when gate (uiop:symbol-call :invite-gate :ensure-invite-cookie! gate req))
                 (funcall handler req ent)
                 ;; named: defined in admin.lisp, after this file
                 (when note? (funcall 'note-activity! req)))))))

(defun publish-invite-door (server)
  "Publish /brickyard/invite/<token>.  The door is a link, so it answers
GET with no body; its own refusal would keep alive and read one."
  (when (gate)
    (let ((prefix "/brickyard/invite/"))
      (net.aserve:publish-prefix
       :prefix prefix :server server
       :function #'(lambda (req ent)
                     (if (bodiless-get-p req)
                         (uiop:symbol-call :invite-gate :invite-door (gate) req ent
                                           (subseq (net.uri:uri-path (net.aserve:request-uri req))
                                                   (length prefix)))
                         (refuse-visitor req ent)))))))

(defun live-gate ()
  (or (gate) (error "Load Gendl's invite-gate first.")))

;; as Stratum and Tala have them
(defun mint-invite! (name) (uiop:symbol-call :invite-gate :mint-invite! (live-gate) name))
(defun revoke-invite! (token) (uiop:symbol-call :invite-gate :revoke-invite! (live-gate) token))
(defun invites-report () (uiop:symbol-call :invite-gate :invites-report (live-gate)))

(defun private-beta (invites-file public-base)
  "Close the gate to all but the invites in invites-file, whose links start with public-base."
  (unless (find-package :invite-gate) (error "Load Gendl's invite-gate first."))
  (setf *gate* (uiop:symbol-call :invite-gate :make-gate
                                 :name "Brickyard" :prefix "/brickyard" :cookie "brickyard_invite"
                                 :invites-file invites-file :public-base public-base))
  (gwl:with-all-servers (server) (publish-invite-door server))
  *gate*)

(defparameter *static-files*
  '(("brickyard.js" . "application/javascript; charset=utf-8")
    ("brickyard.css" . "text/css; charset=utf-8"))
  "List of (name . content type): what /brickyard/static/ serves.")

(defun not-found (req ent)
  (refuse-closing req ent net.aserve:*response-not-found* "text/plain" "Not found."))

(defun static-url (name)
  "Where the page asks for static file name: with the file's date, so that
a cache in front of the server (the edge keeps these four hours) asks
again for a file that changed."
  (format nil "/brickyard/static/~a?v=~a" name
          (or (ignore-errors (file-write-date (merge-pathnames name *static-directory*))) 0)))

(defun static-handler (req ent)
  "The page's script and style, open to all; a published directory, and
AllegroServe's own 404 for anything else, would read a POST's body."
  (let* ((path (net.uri:uri-path (net.aserve:request-uri req)))
         (name (subseq path (length "/brickyard/static/")))
         (type (cdr (assoc name *static-files* :test #'string=))))
    (if (and type (bodiless-get-p req))
        (respond req ent type (uiop:read-file-string (merge-pathnames name *static-directory*)))
        (not-found req ent))))

(defun publish-brickyard (server)
  ;; the proxy sends everything under /brickyard here, published or not
  (net.aserve:publish-prefix :prefix "/brickyard" :server server :function #'not-found)
  (dolist (path '("/brickyard" "/brickyard/"))
    (net.aserve:publish :path path :server server :function (gated #'page-handler)))
  (net.aserve:publish :path "/brickyard/api" :server server :function (gated #'api-handler))
  ;; named, not #': they are defined in api.lisp, after this file
  (net.aserve:publish :path "/brickyard/api/prompt" :server server :function (gated 'prompt-handler))
  (net.aserve:publish :path "/brickyard/api/reference" :server server :function (gated 'reference-handler))
  (net.aserve:publish :path "/brickyard/api/table" :server server :function (gated 'table-handler))
  (net.aserve:publish :path "/brickyard/export" :server server :function (gated #'export-handler))
  (net.aserve:publish :path "/brickyard/import" :server server
                      :function (gated #'import-handler :limit (getf *ldraw-limits* :bytes)))
  (net.aserve:publish-prefix :prefix "/brickyard/static/" :server server :function #'static-handler)
  (publish-invite-door server))

;; Published with the rest when the server starts; and now, on Gendl's
;; own servers (net.aserve:*wserver* need not be the one listening), for
;; a load into a live room.
(pushnew 'publish-brickyard gwl::*publishers*)

(gwl:with-all-servers (server)
  (publish-brickyard server))
