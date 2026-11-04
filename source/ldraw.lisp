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
    (:beam (3 . "32523") (5 . "32316") (7 . "32524") (9 . "40490") (11 . "32525") (15 . "32278"))
    (:axle (3 . "4519") (4 . "3705") (5 . "32073") (6 . "3706") (8 . "3707") (10 . "3737") (12 . "3708"))
    (:pin (nil . "3673"))
    (:gear (8 . "3647") (16 . "4019") (24 . "3648b") (40 . "3649")))
  "Per type, an alist from size key to official part number.  The brick
family's key is the two plan sizes, smaller first.")

(defun matrix* (a b)
  "Product of two 3 by 3 matrices given as lists of rows."
  (let ((columns (apply #'mapcar #'list b)))
    (mapcar #'(lambda (row) (mapcar #'(lambda (column) (dot row column)) columns)) a)))

(defun transpose (m) (apply #'mapcar #'list m))

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
        (:technic-brick
         (let ((number (part (second size))))
           (when number (list (list number *frame-across* '(0 0 24))))))
        (:beam
         (let ((number (part (first size))))
           (when number (list (list number *frame-beam* '(0 0 0))))))
        (:gear
         (let ((number (part (first size))))
           (when number (list (list number *frame-along* '(0 0 0))))))
        ((:axle :pin)
         (let ((number (part (first size))))
           (when number (list (list number *frame-across* '(0 0 0))))))))))

(defun ldraw-line (color rows at frame offset file)
  "A type 1 line placing file, whose frame maps to the spec's by frame
and whose origin sits at offset in it, for a spec turned by rows at at."
  (let ((position (rotate *ldraw-axes* (v+ (rotate rows offset) at)))
        (matrix (matrix* *ldraw-axes* (matrix* rows frame))))
    (format nil "1 ~d ~{~a~^ ~} ~{~a~^ ~} ~a" color position (reduce #'append matrix) file)))

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
  (let* ((rows (part-rotation (spec-up spec) (spec-rear spec)))
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
             (rows (part-rotation (spec-up spec) (spec-rear spec)))
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
