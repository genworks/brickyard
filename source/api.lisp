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

;;
;; The prompt box: a person says in words what to build, a model answers
;; with forms of this API, and Gendl's sexp-agent runs them, the operators
;; above with literal arguments and nothing else.  Without sexp-agent, or
;; until prompting is called, the page has no box.
;;
;;   (brickyard:prompting "http://gate/llm/messages")
;;
;; More than one model may be offered, and the box then has a menu.  The
;; page names one and the server looks it up, so a request can never aim
;; the server at an address of its own:
;;
;;   (brickyard:prompting "http://ollama:11434/v1/messages" :add? t
;;                        :name "Ornith 9B (local)" :model "ornith-1.5:9b")
;;
;; A model can take longer than a proxy holds a request open, so a prompt
;; runs on its own thread and the page asks after it: POST
;; /brickyard/api/prompt with b and action start (and prompt), poll or
;; undo.  One prompt at a time to a build.
;;

(defparameter *prompt-limit* 2000 "Integer. The most characters in a prompt.")

(defparameter *history-turns* 6 "Integer. The earlier turns of a build sent with a prompt.")

(defvar *prompts* (make-hash-table :test #'equal)
  "Hash table from build id to :running, or the finished prompt's reply until it is asked for.")

(defvar *histories* (make-hash-table :test #'equal)
  "Hash table from build id to its turns, (prompt . model's text), oldest first.")

(defvar *before-prompt* (make-hash-table :test #'equal)
  "Hash table from build id to its specs before the last prompt: what undo restores.")

(defun prompting (messages-url &key (name "Opus 5.5") model effort max-tokens call-seconds api-key-file add? turn)
  "Open the prompt box on the model at messages-url: a gate that holds
the key, a local model server, or the API itself with api-key-file.
With add?, offer it beside the models already offered, under name; a
name offered already is replaced.  With turn, a function of one thunk,
every prompt to this model runs inside it, for a model that shares
hardware and has to wait its turn.  Answers the agent."
  (unless (find-package :sexp-agent) (error "Load Gendl's sexp-agent first."))
  (let ((agent (apply #'uiop:symbol-call :sexp-agent :make-agent
                      :name "Brickyard" :package :brickyard-api :operators *operators*
                      :reference #'reference-text :messages-url messages-url
                      :api-key-file api-key-file
                      (append (when model (list :model model))
                              (when effort (list :effort effort))
                              (when max-tokens (list :max-tokens max-tokens))
                              (when call-seconds (list :call-seconds call-seconds))))))
    (setf *agents* (append (when add? (remove name *agents* :key #'first :test #'string=))
                           (list (list name agent (or turn #'funcall)))))
    agent))

(defun find-agent (name)
  "The entry of *agents* offered under name, the first offered for none, or nil."
  (if (and name (plusp (length name)))
      (assoc name *agents* :test #'string=)
      (first *agents*)))

(defun specs-change (before after)
  "Values: the ids in after that are new or differ from before, and the ids gone."
  (values (mapcar #'spec-id (remove-if #'(lambda (s) (member s before :test #'equal)) after))
          (mapcar #'spec-id (remove-if #'(lambda (s) (find (spec-id s) after :key #'spec-id)) before))))

(defun finished-prompt (id prompt answer)
  "The reply to a prompt that has run, with the parts it changed; the turn joins the build's history."
  (let* ((assembly (find-build id))
         ;; a form the repair round put right is no failure
         (failed (unless (getf answer :ok)
                   (find-if-not #'(lambda (r) (getf r :ok)) (getf answer :results) :from-end t)))
         (text (getf answer :assistant)))
    (when text
      (let ((turns (append (gethash id *histories*) (list (cons prompt text)))))
        (setf (gethash id *histories*) (last turns *history-turns*))))
    ;; the whole prompt is one step for the page's undo
    (unless (eq (gethash id *before-prompt*) (the-object assembly part-specs))
      (note-change! id (gethash id *before-prompt*)))
    (multiple-value-bind (changed removed)
        (specs-change (gethash id *before-prompt*) (the-object assembly part-specs))
      (let ((reply (reply assembly :changed changed :removed removed :ok (getf answer :ok)
                                   :message (or (getf answer :error)
                                                (and failed (format nil "Stopped: ~a" (getf failed :error)))))))
        (setf (gethash "reply" reply) (or (getf answer :reply) "")
              (gethash "forms" reply) (coerce (getf answer :forms) 'vector))
        (undo-counts reply id)))))

(defun run-prompt (id prompt session offered)
  "Ask the model offered, an entry of *agents*, run what it answers on
build id, and leave the reply in *prompts*."
  (destructuring-bind (name agent turn) offered
    (declare (ignore name))
    (let* ((*api-build* id)
           (history (bt:with-lock-held (*builds-lock*) (gethash id *histories*)))
           (state (bt:with-lock-held (*builds-lock*) (describe-assembly (find-build id))))
           (answer (handler-case
                       (funcall turn
                                #'(lambda ()
                                    (uiop:symbol-call :sexp-agent :ask agent prompt
                                                      :state-text state :session-id session
                                                      :history history)))
                     (serious-condition (e)
                       (list :ok nil :error (format nil "The model did not answer: ~a" e))))))
      (bt:with-lock-held (*builds-lock*)
        (setf (gethash id *prompts*) (finished-prompt id prompt answer))))))

(defun prompt-session (req id)
  "The id the model's gate counts a person's spending under: their invite, else the build."
  (let ((token (and (gate) (uiop:symbol-call :invite-gate :request-token (gate) req))))
    (if token
        (format nil "brickyard-~(~36r~)" (sxhash token))
        (format nil "brickyard-~a" id))))

(defun start-prompt (req id prompt model)
  (let ((prompt (string-trim '(#\Space #\Tab #\Newline #\Return) (or prompt "")))
        (offered (find-agent model)))
    (cond ((null *agents*) (json-object :ok nil :message "There is no prompt box here."))
          ((null offered) (json-object :ok nil :message "That model is not offered here."))
          ((string= prompt "") (json-object :ok nil :message "Say what to build."))
          ((> (length prompt) *prompt-limit*)
           (json-object :ok nil :message (format nil "A prompt is at most ~a characters." *prompt-limit*)))
          ((eq (gethash id *prompts*) :running)
           (json-object :ok nil :message "A prompt is still running on this build."))
          (t
           (setf (gethash id *prompts*) :running
                 (gethash id *before-prompt*) (the-object (find-build id) part-specs))
           (let ((session (prompt-session req id)))
             (bt:make-thread #'(lambda () (run-prompt id prompt session offered)) :name "brickyard prompt"))
           (json-object :ok t :pending t)))))

(defun poll-prompt (id)
  (let ((state (gethash id *prompts*)))
    (cond ((eq state :running) (json-object :ok t :pending t))
          (state (remhash id *prompts*) state)
          (t (json-object :ok t :idle t)))))

(defun undo-prompt (id)
  "The build as it stood before its last prompt."
  (multiple-value-bind (before found?) (gethash id *before-prompt*)
    (cond ((eq (gethash id *prompts*) :running)
           (json-object :ok nil :message "A prompt is still running on this build."))
          ((not found?) (json-object :ok nil :message "There is no prompt to undo."))
          (t
           (let* ((assembly (find-build id))
                  (now (the-object assembly part-specs)))
             (multiple-value-bind (changed removed) (specs-change now before)
               (note-change! id now)
               (the-object assembly (set-slot! :part-specs before))
               (save-build id assembly)
               (remhash id *before-prompt*)
               (remhash id *histories*)
               (undo-counts (reply assembly :changed changed :removed removed) id)))))))

(defun prompt-handler (req ent)
  "POST b and action: start with prompt, poll, or undo."
  (let* ((params (net.aserve:form-urlencoded-to-query (gwl:request-body-text req)))
         (param #'(lambda (name) (cdr (assoc name params :test #'string=))))
         (id (funcall param "b"))
         (action (funcall param "action"))
         (result (bt:with-lock-held (*builds-lock*)
                   (cond ((null (find-build id)) (json-object :ok nil :message "No such build."))
                         ((equal action "start")
                          (start-prompt req id (funcall param "prompt") (funcall param "model")))
                         ((equal action "poll") (poll-prompt id))
                         ((equal action "undo") (undo-prompt id))
                         (t (json-object :ok nil :message "Unknown action."))))))
    (respond req ent "application/json"
             (with-output-to-string (s) (yason:encode result s)))))

(defun reference-handler (req ent)
  (respond req ent "text/markdown; charset=utf-8" (reference-text)))

;;
;; The table: the build as a robot, a program run on it (table.lisp).  A
;; run is quick, a few hundredths of a second for a minute's driving, and
;; the builds stay unlocked while it goes.  The page plays back the frames.
;;

(defun brickyard-api:run-on-table (program)
  "Stand the current build on the table and run program, a list of steps
such as (:go 300) (:turn 90) (:power 0.5 0.2 2) (:wait 1).  A plist of
what came of it: :steps :robot :heading :seconds :pieces :over :summary."
  (let* ((steps (checked-program program))
         (robot (with-api-build (assembly) (robot-of assembly)))
         (result (run-program robot steps)))
    (list :steps (mapcar #'step-line (getf result :steps))
          :robot (getf result :robot) :heading (getf result :heading)
          :seconds (getf result :seconds) :pieces (getf result :pieces) :over (getf result :over)
          :summary (run-summary result))))

(defun brickyard-api:pybricks-program (program)
  "The text of a Pybricks program for a real hub that does what program
does on the table: the same steps, with the current build's ports,
motor directions, gears, wheel diameter and axle track."
  (let ((steps (checked-program program))
        (robot (with-api-build (assembly) (robot-of assembly))))
    (pybricks-program robot steps :title (format nil "Brickyard build ~a" *api-build*))))

(defun table-json (robot result open?)
  (let ((frames (getf result :frames)))
    (flet ((numbers (list) (map 'vector #'(lambda (x) (float x 1d0)) list)))
      (json-object
       :ok t
       :x3d (and open? (table-x3d))
       :center (numbers (robot-center robot))
       :lift (float (robot-lift robot) 1d0)
       :drives (map 'vector
                    #'(lambda (drive)
                        (map 'vector
                             #'(lambda (part)
                                 (destructuring-bind (id speed point axis) part
                                   ;; about its axis a wheel turns back as it rolls ahead
                                   (json-object :id id
                                                :speed (float (if (plusp (drive-wheels drive)) (- speed) speed) 1d0)
                                                :point (numbers point) :axis (numbers axis))))
                             (drive-parts drive)))
                    (robot-drives robot))
       :frames (if open? (subseq frames 0 1) frames)
       :steps (coerce (mapcar #'step-line (getf result :steps)) 'vector)
       :summary (coerce (run-summary result) 'vector)))))

(defun table-answer (id action text)
  (let* ((open? (equal action "open"))
         (program (if open? nil (checked-program (read-program (or text "")))))
         (robot (bt:with-lock-held (*builds-lock*)
                  (let ((assembly (find-build id)))
                    (if assembly (robot-of assembly) (error "No such build."))))))
    (if (equal action "python")
        (json-object :ok t :python (pybricks-program robot program :title (format nil "Brickyard build ~a" id)))
        (table-json robot (run-program robot program) open?))))

(defun table-handler (req ent)
  "POST b and action: open, run or python, the last two with program."
  (let* ((params (net.aserve:form-urlencoded-to-query (gwl:request-body-text req)))
         (param #'(lambda (name) (cdr (assoc name params :test #'string=))))
         (result (handler-case (table-answer (funcall param "b") (funcall param "action") (funcall param "program"))
                   (serious-condition (e) (json-object :ok nil :message (princ-to-string e))))))
    (respond req ent "application/json"
             (with-output-to-string (s) (yason:encode result s)))))
