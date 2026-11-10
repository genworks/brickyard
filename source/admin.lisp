;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The operator's page, /brickyard/admin: who has an invite and how often
;; they used it, what each visitor did -- visits, changes to a build,
;; runs on the table, prompts, exports, imports -- build by build, and
;; the builds in memory with their prompts and the model's answers.  The
;; record is the activity log, Gendl's visitor-log: one file a visitor,
;; one line a request worth keeping, an imported LDraw file kept beside
;; it.  Off until keep-activity! names a directory, since keeping what
;; visitors do is the operator's decision and something they should be
;; told.  The page changes nothing but what it is asked to delete from
;; the log: a line, or everything kept of a visitor.
;;
;; Only a name on *admins* gets any of it; anyone else, invited or not,
;; is told there is no such page.  No invite token is in anything it
;; answers.  It needs Gendl's visitor-log, loaded as invite-gate is:
;;
;;   (brickyard:keep-activity! "/projects/.state/brickyard-activity/")
;;   (setf brickyard:*admins* '("Name on my invite"))
;;
;; Brickyard compiles without visitor-log (an image built before it):
;; then nothing is kept and there is no admin page.
;;

(defvar *admins* nil
  "Who may see the admin page: a list of invite names; t for anyone the
gate admits, for a development room with no gate; nil for no one, the
default.")

(defvar *activity* nil
  "The activity log, a visitor-log made by keep-activity!; nil when none
is kept.")

(defparameter *admin-events* 300
  "How many of a visitor's lines the admin page is sent unless it asks
for more.")

(defparameter *admin-reply-characters* 6000
  "How much of a model's answer to a prompt the admin page is sent.")

(defun vlog (name &rest args)
  "Gendl's visitor-log function NAME on ARGS, found when called, so this
file compiles in an image that has no visitor-log."
  (apply #'uiop:symbol-call :visitor-log name args))

(defun activity-on? ()
  (and *activity* (find-package :visitor-log) (vlog :log-on? *activity*)))

(defun body-param (req name)
  "NAME in REQ's form body, when a door has read it; nil otherwise."
  (let ((raw (net.aserve::request-request-body req)))
    (and (stringp raw) (plusp (length raw))
         (vlog :query-part (gwl:request-body-text req) name))))

(defun activity-worth-p (req)
  "Whether REQ is worth a line: a visit to the page, an export, and every
POST but the page asking what it shows -- a build's state, what a part
carries, a prompt's progress, the table opened."
  (let ((door (net.uri:uri-path (net.aserve:request-uri req))))
    (if (member (net.aserve:request-method req) '(:get :head))
        (member door '("/brickyard" "/brickyard/" "/brickyard/export") :test #'equal)
        (not (or (and (equal door "/brickyard/api")
                      (member (body-param req "op") '("state" "carried") :test #'equal))
                 (and (equal door "/brickyard/api/prompt") (equal (body-param req "action") "poll"))
                 (and (equal door "/brickyard/api/table") (equal (body-param req "action") "open")))))))

(defun keep-activity! (directory)
  "Keep what each visitor does under DIRECTORY, made if need be and
closed to all but its owner; nothing with nil.  The directory."
  (unless (find-package :visitor-log) (error "Load Gendl's visitor-log first."))
  (unless *activity*
    (setf *activity* (vlog :make-visitor-log :worth 'activity-worth-p
                                             ;; an LDraw file starts "0 "
                                             :upload-types '(("ldr" 48 32)))))
  (vlog :open-log! *activity* directory))

(defun note-activity! (req)
  "A line for REQ in the activity log, after its door has answered; an
import keeps the file it brought."
  (when (activity-on?)
    (let ((raw (net.aserve::request-request-body req)))
      (vlog :note! *activity* (gate) req
            :upload (and (equal (net.uri:uri-path (net.aserve:request-uri req)) "/brickyard/import")
                         (stringp raw) (plusp (length raw))
                         ;; AllegroServe's body is its bytes, one a character
                         (map '(vector (unsigned-byte 8)) #'char-code raw))))))

(defun admin-p (req)
  "Whether REQ's person may see the admin page."
  (and (find-package :visitor-log)
       (vlog :admin? (gate) req *admins*)))

;;; What is kept, summed up.

(defun line-door (line) (or (getf line :door) ""))

(defun line-param (line name)
  "NAME in LINE's query, else in its body."
  (or (vlog :query-part (getf line :query) name)
      (vlog :query-part (getf line :body) name)))

(defun line-kind (line)
  "What a line of the activity log was: :visit, :download, :import,
:prompt (one asked, or undone), :table (a run, or Pybricks), or :change,
anything else done to a build."
  (let ((door (line-door line)))
    (cond ((member door '("/brickyard" "/brickyard/") :test #'equal) :visit)
          ((equal door "/brickyard/export") :download)
          ((equal door "/brickyard/import") :import)
          ((equal door "/brickyard/api/prompt") :prompt)
          ((equal door "/brickyard/api/table") :table)
          (t :change))))

(defun build-rollup (lines)
  "The builds LINES name: each (id :first time :last time :requests n),
the most recent first."
  (let ((builds (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let ((id (line-param line "b"))
            (time (getf line :time)))
        (when (and (stringp id) (plusp (length id)))
          (let ((entry (or (gethash id builds) (list :first time :last time :requests 0))))
            (when (and time (string< time (or (getf entry :first) time))) (setf (getf entry :first) time))
            (when (and time (string> time (or (getf entry :last) ""))) (setf (getf entry :last) time))
            (incf (getf entry :requests))
            (setf (gethash id builds) entry)))))
    (sort (loop for id being the hash-keys of builds using (hash-value entry) collect (cons id entry))
          #'string> :key #'(lambda (entry) (or (getf (cdr entry) :last) "")))))

(defun heap-used-fraction ()
  "How much of the heap is in use, 0 to 1, where the implementation can
say (SBCL); nil elsewhere."
  #+sbcl (/ (float (sb-kernel:dynamic-usage) 1d0) (sb-ext:dynamic-space-size))
  #-sbcl nil)

(defun clipped (text limit)
  (if (and (stringp text) (> (length text) limit))
      (format nil "~a ... (~:d characters in all)" (subseq text 0 limit) (length text))
      text))

(defun builds-summary (whose)
  "The builds in memory, those a visitor touched last first: id, parts,
who, whether a prompt runs, and the prompts with the model's answers."
  (let ((rows (bt:with-lock-held (*builds-lock*)
                (loop for id being the hash-keys of *builds* using (hash-value assembly)
                      collect (list id (length (the-object assembly part-specs))
                                    (eq (gethash id *prompts*) :running)
                                    (copy-list (gethash id *histories*)))))))
    (map 'vector
         #'(lambda (row)
             (destructuring-bind (id parts running turns) row
               (json-object :id id :parts parts
                            :who (first (gethash id whose)) :last (second (gethash id whose))
                            :running running
                            :turns (map 'vector #'(lambda (turn)
                                                    (json-object :prompt (car turn)
                                                                 :reply (clipped (cdr turn) *admin-reply-characters*)))
                                        turns))))
         (stable-sort (sort rows #'string< :key #'first) #'string>
                      :key #'(lambda (row) (or (second (gethash (first row) whose)) ""))))))

(defun admin-summary ()
  "Everyone the roster or the activity log names, summed up, and the
builds in memory: a JSON object for the admin page."
  (let ((visitors (make-hash-table :test 'equal))
        (order '())
        (whose (make-hash-table :test 'equal)))
    (flet ((note (key indicator value &key unless-set)
             (unless (nth-value 1 (gethash key visitors)) (push key order))
             (unless (and unless-set (getf (gethash key visitors) indicator))
               (setf (getf (gethash key visitors) indicator) value))))
      (when (and (gate) (uiop:symbol-call :invite-gate :gate-on? (gate)))
        (dolist (invite (invites-report))
          (let ((key (vlog :visitor-file-name (getf invite :name))))
            (note key :name (getf invite :name))
            (note key :invite invite))))
      (when (activity-on?)
        (dolist (key (vlog :visitor-keys *activity*))
          (let ((lines (vlog :lines *activity* key)))
            (note key :lines lines)
            (note key :name (getf (first lines) :who) :unless-set t)))))
    (let ((rows
            (loop for key in (reverse order)
                  for entry = (gethash key visitors)
                  for lines = (getf entry :lines)
                  for invite = (getf entry :invite)
                  for builds = (build-rollup lines)
                  for times = (remove nil (mapcar #'(lambda (line) (getf line :time)) lines))
                  do (dolist (build builds)
                       (let ((last (getf (cdr build) :last)))
                         (when (string> (or last "") (or (second (gethash (car build) whose)) ""))
                           (setf (gethash (car build) whose) (list (or (getf entry :name) key) last)))))
                  collect (flet ((kind (kind) (count kind lines :key #'line-kind)))
                            (json-object
                             :key key :name (getf entry :name)
                             :invited (and invite t)
                             :issued (getf invite :issued)
                             :uses (and invite (getf invite :uses))
                             :seen (and (integerp (getf invite :last)) (plusp (getf invite :last))
                                        (vlog :utc-stamp (getf invite :last)))
                             :visits (kind :visit) :changes (kind :change) :runs (kind :table)
                             :prompts (kind :prompt) :downloads (kind :download) :imports (kind :import)
                             :first (and times (reduce #'(lambda (a b) (if (string< a b) a b)) times))
                             :last (and times (reduce #'(lambda (a b) (if (string> a b) a b)) times))
                             :builds (map 'vector
                                          #'(lambda (build)
                                              (json-object :id (car build)
                                                           :first (getf (cdr build) :first)
                                                           :last (getf (cdr build) :last)
                                                           :requests (getf (cdr build) :requests)))
                                          builds))))))
      (json-object
       :now (vlog :utc-stamp)
       :server (json-object :lisp (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))
                            :heap (heap-used-fraction)
                            :gate (and (gate) (uiop:symbol-call :invite-gate :gate-on? (gate)) t)
                            :activity (and (activity-on?) t)
                            :models (map 'vector #'first *agents*)
                            :builds (hash-table-count *builds*))
       ;; the most recently active first; then those with nothing
       ;; recorded, by when their link was last used
       :visitors (coerce (stable-sort rows #'string>
                                      :key #'(lambda (row)
                                               (format nil "~a ~a" (or (gethash "last" row) "")
                                                       (or (gethash "seen" row) ""))))
                         'vector)
       :builds (builds-summary whose)))))

;;; One visitor, line by line.

(defun line-json (line)
  "A line of the activity log for the admin page."
  (let ((door (line-door line)))
    (json-object :time (getf line :time)
                 :kind (string-downcase (line-kind line))
                 :door (subseq door (1+ (or (position #\/ door :from-end t) -1)))
                 :build (line-param line "b")
                 :op (or (line-param line "op") (line-param line "action") (line-param line "new"))
                 :prompt (line-param line "prompt")
                 :model (line-param line "model")
                 :program (line-param line "program")
                 :query (getf line :query)
                 :body (getf line :body)
                 :file (getf line :upload)
                 :bytes (getf line :bytes)
                 ;; what the delete door is told to name this line
                 :line (getf line :index)
                 :mark (getf line :mark))))

(defun visitor-key (key)
  "KEY when it is a visitor's file name as the summary gives them, or an
error: nothing else is looked for under the log."
  (unless (and (stringp key) (plusp (length key)) (equal key (vlog :visitor-file-name key)))
    (error "Say which visitor: who, as the summary names them."))
  key)

(defun visitor-events (key &optional (limit *admin-events*))
  "The last LIMIT lines of the visitor whose file is KEY, oldest first:
(values lines how-many-in-all)."
  (let ((lines (and (activity-on?) (vlog :lines *activity* key))))
    (values (loop for line in (last (loop for line in lines
                                          for index from 0
                                          collect (list* :index index :mark (vlog :line-mark line) line))
                                    (max 1 limit))
                  collect (line-json line))
            (length lines))))

;;; The doors.

(defun query-value (req name)
  (net.aserve:request-query-value name req :post nil))

(defun handle-admin-summary (req)
  "GET /brickyard/api/admin: {now, server, visitors, builds}."
  (declare (ignore req))
  (with-output-to-string (s) (yason:encode (admin-summary) s)))

(defun handle-admin-visitor (req)
  "GET /brickyard/api/admin/visitor?who=KEY&limit=N: {who, total, events},
the last N lines kept of one visitor, oldest first."
  (let ((key (query-value req "who"))
        (limit (or (ignore-errors (parse-integer (query-value req "limit"))) *admin-events*)))
    (multiple-value-bind (events total) (visitor-events (visitor-key key) (max 1 (min limit 20000)))
      (with-output-to-string (s)
        (yason:encode (json-object :who key :total total :events (coerce events 'vector)) s)))))

(defun handle-admin-file (req)
  "GET /brickyard/api/admin/file?who=KEY&file=NAME: an LDraw file a
visitor imported, as they sent it."
  (unless (activity-on?) (error "No activity is kept."))
  (let ((path (vlog :kept-file *activity* (visitor-key (query-value req "who")) (query-value req "file"))))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence octets in)
        octets))))

(defun handle-admin-delete (req)
  "POST /brickyard/api/admin/delete, a JSON body naming a visitor as the
summary does and what of theirs is to go: {who: KEY, everything: true},
everything kept of them; {who: KEY, line: N, mark: WORD}, one line, as
the visitor door numbered and marked it, and the file kept with it.
{deleted: {lines, files}}.  Nothing but a POST of JSON is taken, which a
form on someone else's page cannot send."
  (unless (and (eq (net.aserve:request-method req) :post)
               (search "application/json" (or (net.aserve:header-slot-value req :content-type) "")
                       :test #'char-equal))
    (error "Deleting takes a POST of JSON."))
  (unless (activity-on?) (error "No activity is kept."))
  (let* ((body (ignore-errors (yason:parse (gwl:request-body-text req))))
         (key (visitor-key (and (hash-table-p body) (gethash "who" body))))
         (gone (cond ((eq (gethash "everything" body) t)
                      (let ((gone (vlog :delete-visitor! *activity* key)))
                        (list (getf gone :lines) (getf gone :uploads))))
                     ((gethash "line" body)
                      (let ((line (vlog :delete-line! *activity* key (gethash "line" body) (gethash "mark" body))))
                        (list 1 (if (getf line :upload) 1 0))))
                     (t (error "Say what is to go: everything, or a line.")))))
    (with-output-to-string (s)
      (yason:encode (json-object :deleted (json-object :lines (first gone) :files (second gone))) s))))

(defparameter *admin-css* "
html, body { margin: 0; background: #1b1d21; color: #d8dbe0;
  font: 14px/1.45 system-ui, -apple-system, Segoe UI, sans-serif; }
#admin { max-width: 1100px; margin: 0 auto; padding: 12px 16px 60px; }
h1 { font-size: 18px; margin: 4px 0; }
h2 { font-size: 14px; margin: 22px 0 8px; color: #c9cdd3; text-transform: uppercase; letter-spacing: .06em; }
a { color: #8fb8f0; }
button { background: #2c2f35; color: inherit; border: 1px solid #3a3e45; border-radius: 4px;
  padding: 5px 12px; font: inherit; cursor: pointer; }
button:hover { background: #3b5f93; }
button:disabled { opacity: .5; cursor: default; }
button.ad-danger { border-color: #7a3a3a; color: #e0a0a0; }
button.ad-del { grid-column: 4; grid-row: 1; align-self: start; padding: 0 8px; font-size: 12px;
  background: none; border-color: transparent; color: #8a9099; }
button.ad-danger:hover, button.ad-del:hover { background: #7a2e2e; border-color: #7a2e2e; color: #fff; }
.ad-head { display: flex; align-items: center; gap: 12px; flex-wrap: wrap; }
.ad-head h1 { flex: 1; }
.ad-dim { color: #8a9099; }
.ad-mono { font: 12px/1.45 ui-monospace, Menlo, monospace; }
.ad-facts { display: flex; flex-wrap: wrap; gap: 4px 16px; color: #9aa0a8; font-size: 13px; }
.ad-facts b { color: #d8dbe0; font-weight: 600; }
.ad-warn { color: #e0b070; }
.ad-card { background: #222429; border: 1px solid #33363c; border-radius: 6px; margin: 8px 0; }
.ad-card > summary { padding: 9px 12px; cursor: pointer; list-style: none; display: flex;
  flex-wrap: wrap; align-items: baseline; gap: 4px 14px; }
.ad-card > summary::-webkit-details-marker { display: none; }
.ad-card > summary::before { content: '\\25b8'; color: #8a9099; }
.ad-card[open] > summary::before { content: '\\25be'; }
.ad-card[open] > summary { border-bottom: 1px solid #33363c; }
.ad-name { font-weight: 600; font-size: 15px; }
.ad-body { padding: 8px 12px 12px; }
.ad-pill { display: inline-block; border-radius: 9px; padding: 0 8px; font-size: 11px; line-height: 18px;
  background: #2c2f35; color: #c9cdd3; border: 1px solid #3a3e45; white-space: nowrap; }
.ad-pill.ad-live { background: #1f3a2a; border-color: #2f6b45; color: #9fd8b0; }
.ad-pill.ad-model { background: #1f2b3a; border-color: #3b5f93; color: #a8c8f0; }
.ad-event { display: grid; grid-template-columns: 140px 78px 1fr auto; gap: 2px 10px; padding: 5px 0;
  border-top: 1px solid #2c2f35; align-items: baseline; }
.ad-event > .ad-time { color: #8a9099; font-size: 12px; white-space: nowrap; }
.ad-what { min-width: 0; overflow-wrap: anywhere; }
.ad-words { border-left: 3px solid #4a8fe0; background: #1f2b3a; color: #e8eaed; padding: 3px 8px;
  margin: 3px 0; white-space: pre-wrap; }
.ad-reply { border-left: 3px solid #8a9099; padding: 3px 8px; margin: 3px 0; white-space: pre-wrap; }
details.ad-more > summary { cursor: pointer; color: #8a9099; font-size: 12px; }
details.ad-more > pre { margin: 4px 0; white-space: pre-wrap; overflow-wrap: anywhere; color: #c9cdd3;
  background: #17181b; padding: 6px 8px; border-radius: 3px; max-height: 50vh; overflow: auto; }
@media (max-width: 640px) {
  #admin { padding: 10px 12px 60px; }
  .ad-event { grid-template-columns: 1fr auto auto; }
  .ad-event > .ad-what { grid-column: 1 / -1; grid-row: 2; }
  button.ad-del { grid-column: 3; }
}
"
  "The admin page: one column of cards, at a desk or on a phone.")

(defun admin-html ()
  (cl-who:with-html-output-to-string (s nil :prologue "<!doctype html>")
    (:html :lang "en"
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "Brickyard admin")
      (:style (cl-who:str *admin-css*)))
     (:body
      (:div :id "admin" "Loading...")
      (:script :src (format nil "/brickyard/admin.js?v=~a"
                            (or (ignore-errors (file-write-date (merge-pathnames "admin.js" *static-directory*))) 0))
               "")))))

(defun admin-door (content-type fn)
  "A publish function answering FN, a function of the request answering
a string or octets, to someone on *admins*, behind the gate like every
door under /brickyard; anyone else is told what an unpublished path
tells them.  The admin's own looking leaves no line in the log."
  (gated #'(lambda (req ent)
             (if (not (admin-p req))
                 (not-found req ent)
                 (let ((result nil) (failure nil))
                   (handler-case (setf result (funcall fn req))
                     (serious-condition (e) (setf failure (princ-to-string e))))
                   (if failure
                       (respond req ent "text/plain; charset=utf-8" failure
                                :code net.aserve:*response-internal-server-error*)
                       (net.aserve:with-http-response (req ent :content-type content-type)
                         (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
                         (net.aserve:with-http-body (req ent)
                           (let ((out (net.aserve:request-reply-stream req)))
                             (if (stringp result) (write-string result out) (write-sequence result out)))))))))
         :note? nil))

(defun publish-admin (server)
  (flet ((pub (path content-type fn)
           (net.aserve:publish :path path :server server :function (admin-door content-type fn))))
    (pub "/brickyard/admin" "text/html; charset=utf-8" #'(lambda (req) (declare (ignore req)) (admin-html)))
    (pub "/brickyard/admin.js" "application/javascript; charset=utf-8"
         #'(lambda (req) (declare (ignore req))
             (uiop:read-file-string (merge-pathnames "admin.js" *static-directory*))))
    (pub "/brickyard/api/admin" "application/json" #'handle-admin-summary)
    (pub "/brickyard/api/admin/visitor" "application/json" #'handle-admin-visitor)
    (pub "/brickyard/api/admin/file" "text/plain; charset=utf-8" #'handle-admin-file)
    (pub "/brickyard/api/admin/delete" "application/json" #'handle-admin-delete)))

(pushnew 'publish-admin gwl::*publishers*)

(gwl:with-all-servers (server)
  (publish-admin server))
