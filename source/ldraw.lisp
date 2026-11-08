;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; LDraw export: a build as one multi-part document (.mpd) that LDraw
;; programs open.  Brickyard already counts in LDraw units, so what is
;; left is the frames.  LDraw's world is -Y up: its X is ours, its Y is
;; our -Z, its Z is our Y.  Every library part has a frame of its own,
;; read from the official files: bricks, plates and tiles have their
;; origin at the middle of the top face and their long side along X;
;; Technic bricks the same, holes through along Z; beams lie along Z,
;; holes through along Y, origin at the middle hole; axles and pins lie
;; along X, gears turn about Z, all three centered.
;;
;; A part with no library equivalent -- a wheel, the motor, the
;; controller, a brick of a size the library lacks -- goes in the same
;; document as a subfile of its own, drawn from the Gendl model's
;; leaves, so the export shows what the editor shows.  A brick a whole
;; number of bricks high goes out as a stack of standard ones.
;;

(defparameter *ldraw-axes* '((1 0 0) (0 0 -1) (0 1 0))
  "Rows. Brickyard's world to LDraw's.")

(defparameter *frame-across* '((0 0 -1) (1 0 0) (0 -1 0))
  "Rows. A library part's frame to ours when its X runs along our :rear.")

(defparameter *frame-along* '((1 0 0) (0 0 1) (0 -1 0))
  "Rows. A library part's frame to ours when its Z runs along our :rear.")

(defparameter *frame-beam* '((0 1 0) (0 0 1) (1 0 0))
  "Rows. A library beam's frame to ours: it lies along Z with its holes
through along Y, ours along :rear with its holes through along :right.")

(defparameter *ldraw-colors*
  '((:black . 0) (:blue . 1) (:green . 2) (:red . 4) (:brown . 6) (:yellow . 14)
    (:white . 15) (:tan . 19) (:orange . 25) (:lime . 27) (:grey . 71)
    (:grey-light . 151) (:grey-slate-dark . 72))
  "Alist of Brickyard colors to LDraw color codes (LDConfig.ldr).")

(defparameter *ldraw-parts*
  '((:brick ((1 1) . "3005") ((1 2) . "3004") ((1 4) . "3010") ((2 2) . "3003")
     ((2 4) . "3001") ((2 6) . "2456"))
    (:plate ((1 2) . "3023b") ((1 4) . "3710") ((2 2) . "3022") ((2 4) . "3020")
     ((2 6) . "3795") ((2 8) . "3034") ((4 4) . "3031") ((4 6) . "3032")
     ((4 8) . "3035") ((6 10) . "3033"))
    (:tile ((1 2) . "3069b") ((2 2) . "3068b"))
    (:technic-brick (4 . "3701") (6 . "3894") (8 . "3702") (16 . "3703"))
    (:beam (1 . "18654") (2 . "43857") (3 . "32523") (5 . "32316") (7 . "32524") (9 . "40490") (11 . "32525")
     (13 . "41239") (15 . "32278"))
    (:axle (2 . "32062") (3 . "4519") (4 . "3705") (5 . "32073") (6 . "3706") (7 . "44294") (8 . "3707")
     (10 . "3737") (12 . "3708"))
    (:angle-connector (1 . "32013") (2 . "32034") (3 . "32016") (5 . "32015"))
    (:axle-joiner (2 . "59443") (3 . "26287"))
    (:pin-joiner (nil . "62462"))
    (:stop-pin (nil . "32054"))
    (:hole-pin (nil . "87082"))
    (:pin-block (nil . "48989"))
    (:axle-block (nil . "32184"))
    (:t-beam (nil . "60484"))
    (:round-plate (nil . "6141"))
    (:rim (nil . "56145"))
    (:tyre (nil . "15413"))
    (:pin (2 . "3673") (3 . "6558"))
    (:half-pin (nil . "4274"))
    (:axle-pin (1 . "43093") (2 . "11214"))
    (:bush (nil . "3713"))
    (:cross-block (2 . "6536") (3 . "42003"))
    (:l-beam ((3 5) . "32526") ((2 4) . "32140"))
    (:round-tile (nil . "98138"))
    (:gear (8 . "3647") (12 . "32270") (16 . "4019") (20 . "18575") (24 . "3648b") (40 . "3649"))
    (:bent-beam (nil . "6629"))
    (:cheese-slope (nil . "54200"))
    (:split-cross-block (nil . "92907"))
    (:triple-cross-block (nil . "63869"))
    (:twin-cross-block (nil . "41678"))
    (:axle-hub (nil . "10197"))
    (:hole-axle (2 . "22961") (3 . "27940"))
    (:stud-axle (nil . "6587"))
    (:stop-axle (4 . "87083") (8 . "55013"))
    (:axle-connector (nil . "32039"))
    (:axle-beam (nil . "60483"))
    (:h-beam (nil . "14720"))
    (:cross-pin (nil . "15100"))
    (:pin-axle (nil . "18651"))
    (:spike-hub (nil . "45601")) (:spike-large-motor (nil . "54675")) (:spike-medium-motor (nil . "54696"))
    (:spike-color-sensor (nil . "37308")) (:spike-distance-sensor (nil . "37316"))
    (:spike-force-sensor (nil . "37312")) (:spike-wheel (nil . "39367")))
  "Per type, an alist from size key to official part number.  The brick
family's key is the two plan sizes, smaller first; the L-beam's its two
arms; a part of one size has nil.")

(defparameter *frame-same* '((1 0 0) (0 1 0) (0 0 1))
  "Rows. A library part whose frame is ours as it stands: the cross
blocks and the L-beam, whose Brickyard frames were drawn up from theirs.")

(defparameter *ldraw-frames*
  `((:axle ,*frame-across* (0 0 0)) (:pin ,*frame-across* (0 0 0))
    (:half-pin ,*frame-across* (0 0 0)) (:axle-pin ,*frame-across* (0 0 0))
    (:gear ,*frame-along* (0 0 0)) (:bush ,*frame-along* (0 0 0))
    (:beam ,*frame-beam* (0 0 0))
    (:cross-block ,*frame-same* (0 0 0)) (:l-beam ,*frame-same* (0 0 0))
    (:technic-brick ,*frame-across* (0 0 24)) (:round-tile ,*frame-across* (0 0 8))
    (:stop-pin ,*frame-across* (0 0 0)) (:hole-pin ,*frame-across* (0 0 0)) (:pin-joiner ,*frame-across* (0 0 0))
    (:axle-joiner ,*frame-along* (0 0 0)) (:rim ,*frame-along* (0 0 0)) (:tyre ,*frame-along* (0 0 0))
    (:angle-connector ,*frame-same* (0 0 0)) (:pin-block ,*frame-same* (0 0 0))
    (:axle-block ,*frame-same* (0 0 0)) (:t-beam ,*frame-same* (0 0 0))
    (:round-plate ,*frame-across* (0 0 8))
    (:stop-axle ,*frame-across* (0 0 0)) (:cheese-slope ,*frame-across* (0 0 0))
    ,@(mapcar #'(lambda (type) (list type *frame-same* '(0 0 0)))
              '(:bent-beam :split-cross-block :triple-cross-block :twin-cross-block :axle-hub :hole-axle
                :stud-axle :axle-connector :axle-beam :h-beam :cross-pin :pin-axle
                ;; the SPIKE parts' frames were read from the library's
                :spike-hub :spike-large-motor :spike-medium-motor :spike-color-sensor
                :spike-distance-sensor :spike-force-sensor :spike-wheel)))
  "Per type outside the brick family, (frame offset) of its library parts,
read from the library files: pins and axle-pins lie along X from their
collar, the bush and gears turn about Z, beams hold their holes along Y.")

(defun ldraw-key (type size)
  "The key a part of this type and size has in *ldraw-parts*."
  (case type
    ((:half-pin :bush :round-tile :pin-joiner :stop-pin :hole-pin :pin-block :axle-block :t-beam
      :round-plate :rim :tyre :spike-hub :spike-large-motor :spike-medium-motor :spike-color-sensor
      :spike-distance-sensor :spike-force-sensor :spike-wheel)
     nil)
    (:l-beam size)
    (:technic-brick (second size))
    (t (first size))))


(defun ldraw-color (color)
  (or (cdr (assoc color *ldraw-colors*)) 71))

(defun ldraw-placements (spec)
  "List of (part-number frame offset), offset in the spec's own LDU, for
the library parts that make up the spec; nil when there are none."
  (let* ((type (spec-type spec))
         (size (spec-size spec))
         (table (rest (assoc type *ldraw-parts*))))
    (flet ((part (key) (cdr (assoc key table :test #'equal))))
      (case type
        ((:brick :plate :tile)
         (destructuring-bind (w l h) size
           (let ((number (part (sort (list w l) #'<)))
                 (frame (if (>= l w) *frame-across* *frame-along*)))
             (cond ((null number) nil)
                   ((eql type :brick)
                    (when (zerop (mod h 3))
                      (mapcar #'(lambda (k) (list number frame (list 0 0 (* 24 (1+ k)))))
                              (iota (/ h 3)))))
                   (t (list (list number frame (list 0 0 8))))))))
        (t
         (let ((number (part (ldraw-key type size)))
               (frame (rest (assoc type *ldraw-frames*))))
           (when (and number frame)
             (list (list number (first frame) (second frame))))))))))

(defun ldraw-line (color rows at frame offset file)
  "A type 1 line placing file, whose frame maps to the spec's by frame
and whose origin sits at offset in it, for a spec turned by rows at at."
  (let ((position (rotate *ldraw-axes* (v+ (rotate rows offset) at)))
        (matrix (matrix* *ldraw-axes* (matrix* rows frame))))
    (format nil "1 ~d ~{~a~^ ~} ~{~a~^ ~} ~a" color
            (mapcar #'ldraw-text position) (mapcar #'ldraw-text (reduce #'append matrix)) file)))

(defun ldraw-text (x)
  "A number as an LDraw file writes it: whole numbers plain, others to six places."
  (if (integerp x)
      (format nil "~d" x)
      (let ((text (format nil "~,6f" x)))
        (string-right-trim "." (string-right-trim "0" text)))))

(defun custom-name (spec)
  (format nil "brickyard-~(~a~)~@[-~{~a~^x~}~].dat" (spec-type spec) (spec-size spec)))

;;
;; Custom subfiles, from the Gendl part's leaves
;;

(defun vector-list (v) (list (get-x v) (get-y v) (get-z v)))

(defun ldraw-number (x)
  (let ((text (format nil "~,3f" x)))
    (string-right-trim "." (if (find #\. text) (string-right-trim "0" text) text))))

(defun custom-geometry (part spec)
  "List of LDraw quad and triangle lines drawing part, a Gendl object
standing where spec says, in its own frame turned into LDraw's axes."
  (let* ((rows (spec-rotation spec))
         (back (transpose rows))
         (at (spec-at spec))
         (color (spec-color spec))
         lines)
    (labels ((local (point)
               ;; world mm -> the part's own LDU -> LDraw axes
               (rotate *ldraw-axes* (rotate back (v- (mapcar #'(lambda (c) (/ c *mm-per-ldu*)) point) at))))
             (emit (kind code points)
               (push (format nil "~d ~d ~{~a~^ ~}" kind code
                             (mapcar #'ldraw-number (reduce #'append (mapcar #'local points))))
                     lines))
             (offset (point &rest terms)
               ;; point plus each (scale vector) in terms
               (reduce #'(lambda (p term) (mapcar #'(lambda (a b) (+ a (* (first term) b))) p (second term)))
                       terms :initial-value point)))
      (dolist (leaf (the-object part leaves) (nreverse lines))
        (let* ((leaf-color (getf (the-object leaf display-controls) :color))
               (code (if (or (null leaf-color) (eql leaf-color color)) 16 (ldraw-color leaf-color)))
               (center (vector-list (the-object leaf center)))
               (right (vector-list (the-object leaf (face-normal-vector :right))))
               (rear (vector-list (the-object leaf (face-normal-vector :rear))))
               (top (vector-list (the-object leaf (face-normal-vector :top)))))
          (cond
            ((typep leaf 'box)
             (let ((w (half (the-object leaf width))) (l (half (the-object leaf length))) (h (half (the-object leaf height))))
               (flet ((corner (i j k) (offset center (list (* i w) right) (list (* j l) rear) (list (* k h) top))))
                 (dolist (face '(((1 -1 -1) (1 1 -1) (1 1 1) (1 -1 1)) ((-1 -1 -1) (-1 -1 1) (-1 1 1) (-1 1 -1))
                                 ((-1 1 -1) (-1 1 1) (1 1 1) (1 1 -1)) ((-1 -1 -1) (1 -1 -1) (1 -1 1) (-1 -1 1))
                                 ((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)) ((-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1))))
                   (emit 4 code (mapcar #'(lambda (c) (apply #'corner c)) face))))))
            ((typep leaf 'cylinder)
             (let* ((r (the-object leaf radius)) (l (half (the-object leaf length))) (n 16)
                    (rim (mapcar #'(lambda (k) (let ((a (/ (* 2 pi k) n)))
                                                 (list (* r (cos a)) (* r (sin a)))))
                                 (iota (1+ n)))))
               (flet ((at-rim (cs side) (offset center (list (first cs) right) (list (second cs) top) (list side rear))))
                 (mapc #'(lambda (a b)
                           (emit 4 code (list (at-rim a (- l)) (at-rim b (- l)) (at-rim b l) (at-rim a l)))
                           (emit 3 code (list (offset center (list (- l) rear)) (at-rim b (- l)) (at-rim a (- l))))
                           (emit 3 code (list (offset center (list l rear)) (at-rim a l) (at-rim b l))))
                       rim (rest rim)))))))))))

;;
;; The document
;;

(defun to-ldraw (assembly &key (name "brickyard"))
  "String. The build as an LDraw multi-part document: the model, then a
subfile for each kind of part the official library has no match for."
  (let ((specs (the-object assembly part-specs))
        (customs nil)
        (model nil))
    (dotimes (i (length specs))
      (let* ((spec (nth i specs))
             (rows (spec-rotation spec))
             (color (ldraw-color (spec-color spec)))
             (placements (ldraw-placements spec)))
        (if placements
            (dolist (p placements)
              (destructuring-bind (number frame offset) p
                (push (ldraw-line color rows (spec-at spec) frame offset (format nil "~a.dat" number)) model)))
            (let ((file (custom-name spec)))
              (unless (assoc file customs :test #'string=)
                (push (cons file (custom-geometry (the-object assembly (parts i)) spec)) customs))
              (push (ldraw-line color rows (spec-at spec) (transpose *ldraw-axes*) '(0 0 0) file) model)))))
    (with-output-to-string (s)
      (format s "0 FILE ~a.ldr~%0 ~a~%0 Name: ~a.ldr~%0 Author: Brickyard~%~{~a~%~}0 NOFILE~%"
              name name name (nreverse model))
      (dolist (custom (nreverse customs))
        (format s "0 FILE ~a~%0 ~a (Brickyard)~%0 Name: ~a~%0 Author: Brickyard~%0 !LDRAW_ORG Unofficial_Part~%~{~a~%~}0 NOFILE~%"
                (first custom) (subseq (first custom) 0 (- (length (first custom)) 4)) (first custom) (rest custom))))))

;;
;; LDraw import: the inverse.  A library part is known by its number
;; (older numbers of the same part too), a Brickyard subfile by its
;; name; submodels are flattened through their placements, and color
;; 16 takes the color of the line that placed it.  A part at an angle
;; off the square grid comes in at that angle; a part Brickyard does
;; not have, or one placed as a mirror image, is left out and counted.
;; The text is untrusted: numbers are read by hand, never by
;; the Lisp reader, and its size and part count are capped.
;;

(defparameter *ldraw-aliases*
  '(("3023" . "3023b") ("3648" . "3648b") ("3648a" . "3648b") ("3069" . "3069b")
    ("3069a" . "3069b") ("3068" . "3068b") ("3068a" . "3068b") ("2780" . "3673"))
  "Alist of older or variant part numbers to the ones *ldraw-parts* names.")

(defparameter *ldraw-color-names*
  '((7 . :grey) (8 . :grey-slate-dark) (16 . :grey) (70 . :brown)
    ;; rubber, as tyres and bands come (LDConfig.ldr)
    (256 . :black) (511 . :white) (324 . :red) (273 . :blue) (65 . :yellow) (350 . :orange)
    (490 . :lime) (375 . :grey) (496 . :grey) (10072 . :grey-slate-dark))
  "Alist of LDraw color codes beyond *ldraw-colors* to Brickyard colors.")

(defparameter *ldraw-limits* '(:bytes 4000000 :parts 2000 :depth 8 :visits 50000)
  "Plist. The most text, parts, submodel depth and placements visited an
import will take: a submodel that places itself ten times over is
cut off by :visits long before its depth runs out.  Two thousand parts
survey in about half a second, which every edit of the build repeats.")

(defun import-note (count report)
  "String. What an import brought in and what it left out, for the person who asked."
  (with-output-to-string (s)
    (when (plusp count) (format s "Imported ~d part~:p." count))
    (let ((unknown (getf report :unknown)))
      (when unknown
        (format s " Left out ~d part~:p Brickyard does not have:~{ ~a~}~:[~;, and more~]."
                (reduce #'+ unknown :key #'rest)
                (mapcar #'(lambda (e) (format nil "~a x~d" (first e) (rest e)))
                        (subseq unknown 0 (min 5 (length unknown))))
                (> (length unknown) 5))))
    (when (plusp (getf report :at-angle 0))
      (format s " ~d stand at an angle, as placed." (getf report :at-angle)))
    (when (plusp (getf report :mirrored 0))
      (format s " Left out ~d placed as mirror images." (getf report :mirrored)))
    (when (plusp (getf report :straightened 0))
      (format s " Set square a slight tilt the model was shown at."))
    (when (plusp (getf report :drawn 0))
      (format s " Skipped ~d pieces of drawn geometry (springs, ropes and the like), which are not parts."
              (getf report :drawn)))
    (when (plusp (getf report :recolored))
      (format s " ~d in colors Brickyard lacks became grey." (getf report :recolored)))
    (when (plusp (getf report :unreadable))
      (format s " ~d line~:p could not be read." (getf report :unreadable)))
    (when (getf report :cut)
      (format s " The file was cut off at Brickyard's limit of ~d parts." (getf *ldraw-limits* :parts)))))

(defun ldraw-catalog ()
  "Hash table from a part number to (type size frame offset)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry *ldraw-parts* table)
      (let ((type (first entry)))
        (dolist (pair (rest entry))
          (destructuring-bind (key . number) pair
            (setf (gethash number table)
                  (case type
                    (:brick (list type key *frame-across* '(0 0 24)))
                    ((:plate :tile) (list type key *frame-across* '(0 0 8)))
                    (t (destructuring-bind (frame offset) (rest (assoc type *ldraw-frames*))
                         (list type
                               (cond ((null key) nil) ((listp key) key) (t (list key)))
                               frame offset)))))))))))

(defun parse-decimal (string)
  "Rational. A decimal number like -12, 0.5 or 1e-3 read exactly, or nil."
  (let ((n (length string)) (i 0) (sign 1) (mantissa 0) (scale 0) (digits 0) (exponent 0) (exponent-sign 1))
    (flet ((digits-while (f) (do () ((or (>= i n) (not (digit-char-p (char string i)))))
                               (funcall f (digit-char-p (char string i))) (incf i))))
      (when (and (< i n) (find (char string i) "+-"))
        (when (char= (char string i) #\-) (setf sign -1))
        (incf i))
      (digits-while #'(lambda (d) (setf mantissa (+ (* 10 mantissa) d)) (incf digits)))
      (when (and (< i n) (char= (char string i) #\.))
        (incf i)
        (digits-while #'(lambda (d) (setf mantissa (+ (* 10 mantissa) d)) (incf digits) (incf scale))))
      (when (and (< i n) (char-equal (char string i) #\e) (> digits 0))
        (incf i)
        (when (and (< i n) (find (char string i) "+-"))
          (when (char= (char string i) #\-) (setf exponent-sign -1))
          (incf i))
        (digits-while #'(lambda (d) (setf exponent (+ (* 10 exponent) d)))))
      (when (and (= i n) (< 0 digits 30) (<= exponent 30))
        (* sign mantissa (expt 10 (- (* exponent-sign exponent) scale)))))))

(defun split-blanks (line)
  "List of the strings in line between spaces and tabs."
  (let (tokens (start nil))
    (dotimes (i (length line))
      (if (member (char line i) '(#\Space #\Tab))
          (when start (push (subseq line start i) tokens) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq line start) tokens))
    (nreverse tokens)))

(defun ldraw-name (string)
  "A file reference as compared: lower case, forward slashes, no folder prefix."
  (let ((name (string-downcase (substitute #\/ #\\ (string-trim '(#\Space #\Tab #\Return) string)))))
    (dolist (prefix '("parts/" "p/") name)
      (when (and (> (length name) (length prefix)) (string= prefix (subseq name 0 (length prefix))))
        (setf name (subseq name (length prefix)))))))

(defun ldraw-sections (text)
  "List of (name . lines), in order, one for each FILE of a multi-part
document; one section named \"\" when the text has none."
  (let ((sections nil) (current nil))
    (dolist (raw (uiop:split-string text :separator (list #\Newline)))
      (let* ((line (string-trim '(#\Space #\Tab #\Return) raw))
             (tokens (split-blanks line)))
        (cond ((and (>= (length tokens) 3) (string= (first tokens) "0") (string-equal (second tokens) "FILE"))
               (when current (push current sections))
               (setf current (list (ldraw-name (subseq line (+ (search (second tokens) line) 4))))))
              ((and (= (length tokens) 2) (string= (first tokens) "0") (string-equal (second tokens) "NOFILE"))
               (when current (push current sections) (setf current nil)))
              (t (unless current (setf current (list "")))
                 (push tokens (rest current))))))
    (when current (push current sections))
    (mapcar #'(lambda (s) (cons (first s) (reverse (rest s)))) (nreverse sections))))

(defun custom-spec-kind (name)
  "(type size) from a Brickyard subfile name such as brickyard-wheel-5x2.dat, or nil."
  (let ((prefix "brickyard-") (suffix ".dat"))
    (when (and (> (length name) (+ (length prefix) (length suffix)))
               (string= prefix (subseq name 0 (length prefix)))
               (string= suffix (subseq name (- (length name) (length suffix)))))
      (let* ((stem (subseq name (length prefix) (- (length name) (length suffix))))
             (type (find-if #'(lambda (k) (let ((word (string-downcase (symbol-name k))))
                                            (and (<= (length word) (length stem))
                                                 (string= word (subseq stem 0 (length word)))
                                                 (or (= (length word) (length stem))
                                                     (char= (char stem (length word)) #\-)))))
                            ;; longest first, so technic-brick is not read as brick
                            (sort (copy-list *part-types*) #'> :key #'(lambda (k) (length (symbol-name k))))))
             (rest (and type (string-left-trim "-" (subseq stem (length (symbol-name type))))))
             (size (and type (if (string= rest "")
                                 nil
                                 (mapcar #'parse-decimal (uiop:split-string rest :separator "x"))))))
        (when (and type (every #'(lambda (n) (and (integerp n) (< 0 n 100))) size))
          (list type size))))))

(defun square-matrix (rows &optional (tolerance 1/1000))
  "The rows rounded to an integer rotation when every entry is within
tolerance of -1, 0 or 1 and the result turns without mirroring; else nil."
  (let ((rounded (mapcar #'(lambda (row) (mapcar #'round row)) rows)))
    (when (and (every #'(lambda (row r) (every #'(lambda (x y) (< (abs (- x y)) tolerance)) row r)) rows rounded)
               (every #'(lambda (row) (= 1 (count-if-not #'zerop row))) rounded)
               (equal (rotate rounded '(0 0 1))
                      (cross (rotate rounded '(1 0 0)) (rotate rounded '(0 1 0)))))
      rounded)))

(defparameter *presentation-tilt* 9/50
  "Number. A placement in the main model whose matrix is within this of a
square turn in every entry -- about ten degrees -- is taken for a pose
the author gave the whole model to show it off, and set square.")

(defun drawn-geometry-p (name)
  "True when a referenced file looks like an LDraw primitive (4-4cyli,
rect2p, box2-7, stud4 ...) rather than a part: drawing, not a brick.
Parts are known by a run of three or more digits (3001, 32062, u9006)."
  (let* ((stem (if (and (> (length name) 4) (string= ".dat" (subseq name (- (length name) 4))))
                   (subseq name 0 (- (length name) 4))
                   name))
         (stem (subseq stem (1+ (or (position #\/ stem :from-end t) -1))))
         (run 0) (longest 0))
    (dotimes (i (length stem))
      (if (digit-char-p (char stem i))
          (setf longest (max longest (incf run)))
          (setf run 0)))
    (and (plusp (length stem))
         (or (and (alpha-char-p (char stem 0)) (< longest 3))
             (let ((dash (position #\- stem)))
               (and dash (plusp dash) (every #'digit-char-p (subseq stem 0 dash))
                    (< dash (1- (length stem))) (digit-char-p (char stem (1+ dash)))))))))

(defun orthonormal (rows)
  "The rotation nearest rows, made exactly orthonormal (Gram-Schmidt on
its columns, as double floats), or nil when rows mirror or collapse."
  (let* ((c0 (mapcar #'(lambda (x) (coerce x 'double-float)) (column rows 0)))
         (c1 (mapcar #'(lambda (x) (coerce x 'double-float)) (column rows 1)))
         (c2 (column rows 2)))
    (when (and (> (norm c0) 1d-6) (> (norm c1) 1d-6))
      (let* ((x (unit c0))
             (y (unit (v- c1 (mapcar #'(lambda (e) (* (dot c1 x) e)) x))))
             (z (cross x y)))
        (when (plusp (dot z c2))
          (mapcar #'list x y z))))))

(defun thousandth (x) (/ (round (* x 1000)) 1000d0))

(defun color-from-ldraw (code)
  (or (car (rassoc code *ldraw-colors*)) (cdr (assoc code *ldraw-color-names*))))

(defun from-ldraw (text)
  "Two values: a list of part specs read from LDraw text (.ldr or .mpd),
and a plist reporting what was left out: :unknown, an alist of part
names to counts; :mirrored, parts placed as mirror images, left out;
:at-angle, parts kept at an angle off the square grid; :recolored, parts
whose color Brickyard lacks; :submodels, submodels flattened; :cut,
true when a limit stopped the reading; :too-big, true when the text
was too long to read at all."
  (let* ((limits *ldraw-limits*)
         (sections (when (<= (length text) (getf limits :bytes)) (ldraw-sections text)))
         (catalog (ldraw-catalog))
         (specs nil) (unknown nil) (mirrored 0) (at-angle 0) (recolored 0) (submodels 0) (count 0) (visits 0)
         (cut nil) (unreadable 0) (drawn 0) (straightened 0))
    (labels ((place (tokens parent-color parent-p parent-a depth)
               (when (> (incf visits) (getf limits :visits)) (setf cut t) (return-from place))
               (unless (>= (length tokens) 15) (incf unreadable) (return-from place))
               (let ((numbers (mapcar #'parse-decimal (subseq tokens 1 14))))
                 (when (some #'null numbers) (incf unreadable) (return-from place))
                 (let* ((file (ldraw-name (format nil "~{~a~^ ~}" (nthcdr 14 tokens))))
                        (code (first numbers))
                        (code (if (member code '(16 24)) parent-color code))
                        (p (v+ parent-p (rotate parent-a (subseq numbers 1 4))))
                        (a (matrix* parent-a (list (subseq numbers 4 7) (subseq numbers 7 10) (subseq numbers 10 13))))
                        (number (let ((stem (if (and (> (length file) 4) (string= ".dat" (subseq file (- (length file) 4))))
                                                (subseq file 0 (- (length file) 4))
                                                file)))
                                  (or (cdr (assoc stem *ldraw-aliases* :test #'string=)) stem)))
                        (known (gethash number catalog))
                        (custom (custom-spec-kind file))
                        (section (assoc file sections :test #'string=)))
                   (cond ((or known custom)
                          (destructuring-bind (type size frame offset)
                              (or known (list (first custom) (second custom) (transpose *ldraw-axes*) '(0 0 0)))
                            (let* ((raw (matrix* (transpose *ldraw-axes*) (matrix* a (transpose frame))))
                                   (rows (square-matrix raw))
                                   (angled (and (null rows) (orthonormal raw)))
                                   (color (color-from-ldraw code)))
                              (cond ((and (null rows) (null angled)) (incf mirrored))
                                    ((> (incf count) (getf limits :parts)) (setf cut t))
                                    (t
                                     (unless color (incf recolored))
                                     (push (if rows
                                               (list :type type :size size
                                                     :at (mapcar #'round (v- (rotate (transpose *ldraw-axes*) p) (rotate rows offset)))
                                                     :up (axis-key (rotate rows '(0 0 1)))
                                                     :rear (axis-key (rotate rows '(0 1 0)))
                                                     :color (or color :grey))
                                               (progn
                                                 (incf at-angle)
                                                 (list :type type :size size
                                                       :at (mapcar #'thousandth (v- (rotate (transpose *ldraw-axes*) p) (rotate angled offset)))
                                                       :matrix angled
                                                       :color (or color :grey))))
                                           specs))))))
                         ((and section (< depth (getf limits :depth)))
                          (incf submodels)
                          (dolist (child (rest section))
                            (when (string= (first child) "1")
                              (place child code p a (1+ depth)))))
                         ((drawn-geometry-p file) (incf drawn))
                         (t (let ((entry (assoc file unknown :test #'string=)))
                              (if entry (incf (rest entry)) (push (cons file 1) unknown)))))))))
      (dolist (tokens (rest (first sections)))
        (when (and tokens (string= (first tokens) "1"))
          ;; a slight tilt of the whole model, given to show it off, is set square
          (let* ((numbers (and (>= (length tokens) 15) (mapcar #'parse-decimal (subseq tokens 5 14))))
                 (rows (and numbers (notany #'null numbers)
                            (list (subseq numbers 0 3) (subseq numbers 3 6) (subseq numbers 6 9)))))
            (when (and rows (not (square-matrix rows)) (square-matrix rows *presentation-tilt*))
              (incf straightened)
              (setf tokens (append (subseq tokens 0 5)
                                   (mapcar #'princ-to-string (reduce #'append (square-matrix rows *presentation-tilt*)))
                                   (nthcdr 14 tokens)))))
          (place tokens 16 '(0 0 0) '((1 0 0) (0 1 0) (0 0 1)) 0))))
    (values (nreverse specs)
            (list :unknown (sort unknown #'> :key #'rest) :mirrored mirrored :at-angle at-angle :recolored recolored
                  :submodels submodels :unreadable unreadable :cut cut :drawn drawn
                  :straightened straightened
                  :too-big (> (length text) (getf limits :bytes))))))
