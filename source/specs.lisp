;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :brickyard)

;;
;; The data layer: a build is a list of part specs, plain plists, and
;; everything that decides whether parts fit -- connectors, extents,
;; mates, collisions, snapping -- is a pure function of them.  The
;; Gendl objects in parts.lisp draw from the same connectors, so the
;; picture and the rules cannot disagree.
;;
;; Positions are integers in LDraw units (LDU): a stud pitch is 20, a
;; plate 8, a brick 24, and one LDU is 0.4 mm.  Rotations are the 24
;; axis-aligned ones, as integer matrices, so every comparison is exact.
;; Axes follow Gendl's cube: X :right, Y :rear, Z :top.
;;
;; A spec:  (:type :brick :size (2 4) :at (0 0 0) :up :top :rear :rear :color :red)
;;
;; :type   :brick (w l [plates-high, default 3]), :plate (w l), :tile (w l),
;;         :technic-brick (l), :beam (holes), :axle (studs long), :pin ()
;; :at     the part's anchor in LDU: the center of the bottom face for
;;         bricks, plates and tiles, the middle of the part otherwise
;; :up :rear   where the part's own :top and :rear face, as axis keywords
;;

(defparameter *mm-per-ldu* 0.4d0 "Number. Millimeters in one LDraw unit.")

(defparameter *snap-radius* 60
  "Integer. How far, in LDU, a connector may be from a mating one and
still pull the part onto it (three studs).")

(defparameter *axes*
  '((:right 1 0 0) (:left -1 0 0)
    (:rear 0 1 0) (:back 0 1 0) (:front 0 -1 0)
    (:top 0 0 1) (:up 0 0 1) (:bottom 0 0 -1) (:down 0 0 -1)))

(defparameter *default-colors*
  '(:brick :red :plate :blue :tile :yellow :technic-brick :grey
    :beam :grey-light :axle :black :pin :black))

(defun mm (ldu) (* ldu *mm-per-ldu*))

(defun axis (key)
  (or (rest (assoc key *axes*)) (error "~s is not an axis keyword." key)))

(defun axis-key (v)
  (first (find v *axes* :key #'rest :test #'equal)))

(defun iota (n)
  (let (result) (dotimes (i n (nreverse result)) (push i result))))

(defun v+ (a b) (mapcar #'+ a b))
(defun v- (a b) (mapcar #'- a b))
(defun dot (a b) (reduce #'+ (mapcar #'* a b)))
(defun cross (a b)
  (destructuring-bind (a1 a2 a3) a
    (destructuring-bind (b1 b2 b3) b
      (list (- (* a2 b3) (* a3 b2)) (- (* a3 b1) (* a1 b3)) (- (* a1 b2) (* a2 b1))))))
(defun zero-vector-p (v) (every #'zerop v))

(defun part-rotation (up rear)
  "List of three rows: the matrix taking a part's own axes to the world,
its columns the world directions of the part's :right, :rear and :top."
  (let* ((z (axis up)) (y (axis rear)) (x (cross y z)))
    (unless (zerop (dot y z))
      (error "A part cannot face both ~s up and ~s to the rear." up rear))
    (mapcar #'list x y z)))

(defun rotate (rows v) (mapcar #'(lambda (row) (dot row v)) rows))

(defun collect-pairs (function items)
  "List of the non-nil values of function on (i j item-i item-j), i < j."
  (let (result)
    (do ((rest items (rest rest)) (i 0 (1+ i))) ((null rest) (nreverse result))
      (do ((tail (rest rest) (rest tail)) (j (1+ i) (1+ j))) ((null tail))
        (let ((value (funcall function i j (first rest) (first tail))))
          (when value (push value result)))))))

;;
;; Specs
;;

(defun spec-type (spec) (getf spec :type))
(defun spec-at (spec) (or (getf spec :at) (list 0 0 0)))
(defun spec-up (spec) (or (getf spec :up) :top))
(defun spec-rear (spec) (or (getf spec :rear) :rear))
(defun spec-color (spec) (or (getf spec :color) (getf *default-colors* (spec-type spec))))

(defun spec-size (spec)
  "List. The size filled out to what the type needs: (w l plates) for the
brick family, (n) for beams and axles, () for a pin."
  (let ((size (getf spec :size)))
    (ecase (spec-type spec)
      (:brick (list (first size) (second size) (or (third size) 3)))
      ((:plate :tile) (list (first size) (second size) 1))
      (:technic-brick (list 1 (first size) 3))
      ((:beam :axle) (list (first size)))
      (:pin nil))))

(defun move-spec (spec at)
  (let ((new (copy-list spec))) (setf (getf new :at) at) new))

;;
;; Connectors: where a part joins another.  :stud and :socket mate
;; face to face; :pin, :axle and the holes mate along a shared axis.
;; half is the connector's half-length along that axis.
;;

(defstruct connector kind point dir (half 0))

(defun row-offset (j n)
  "LDU from the middle of a row of n places, 20 apart, to place j."
  (- (* 20 j) (* 10 (1- n))))

(defun local-connectors (spec)
  (let ((size (spec-size spec)) (type (spec-type spec)))
    (ecase type
      ((:brick :plate :tile :technic-brick)
       (destructuring-bind (w l h) size
         (let (result)
           (dotimes (i w)
             (dotimes (j l)
               (let ((x (row-offset i w)) (y (row-offset j l)))
                 (unless (eql type :tile)
                   (push (make-connector :kind :stud :point (list x y (* 8 h)) :dir '(0 0 1)) result))
                 (push (make-connector :kind :socket :point (list x y 0) :dir '(0 0 -1)) result))))
           (when (eql type :technic-brick)
             (dotimes (j (1- l))
               (push (make-connector :kind :pin-hole :point (list 0 (row-offset j (1- l)) 14)
                                     :dir '(1 0 0) :half 10)
                     result)))
           (nreverse result))))
      (:beam
       (let ((n (first size)))
         (mapcar #'(lambda (j) (make-connector :kind :pin-hole :point (list 0 (row-offset j n) 0)
                                               :dir '(1 0 0) :half 9))
                 (iota n))))
      (:axle
       (list (make-connector :kind :axle :point '(0 0 0) :dir '(0 1 0) :half (* 10 (first size)))))
      (:pin
       (list (make-connector :kind :pin :point '(0 -10 0) :dir '(0 1 0) :half 10)
             (make-connector :kind :pin :point '(0 10 0) :dir '(0 1 0) :half 10))))))

(defun local-extent (spec)
  "List of two corners, low and high, of the part's own box in LDU, studs left out."
  (let ((size (spec-size spec)))
    (ecase (spec-type spec)
      ((:brick :plate :tile :technic-brick)
       (destructuring-bind (w l h) size
         (list (list (* -10 w) (* -10 l) 0) (list (* 10 w) (* 10 l) (* 8 h)))))
      (:beam (let ((y (+ (* 10 (1- (first size))) 9))) (list (list -9 (- y) -9) (list 9 y 9))))
      (:axle (let ((y (* 10 (first size)))) (list (list -6 (- y) -6) (list 6 y 6))))
      (:pin (list '(-6 -20 -6) '(6 20 6))))))

(defun world-connectors (spec)
  (let ((rows (part-rotation (spec-up spec) (spec-rear spec))) (at (spec-at spec)))
    (mapcar #'(lambda (c)
                (make-connector :kind (connector-kind c) :half (connector-half c)
                                :point (v+ at (rotate rows (connector-point c)))
                                :dir (rotate rows (connector-dir c))))
            (local-connectors spec))))

(defun world-extent (spec)
  (let* ((rows (part-rotation (spec-up spec) (spec-rear spec)))
         (at (spec-at spec))
         (corners (mapcar #'(lambda (p) (rotate rows p)) (local-extent spec))))
    (list (v+ at (apply #'mapcar #'min corners)) (v+ at (apply #'mapcar #'max corners)))))

;;
;; Mates
;;

(defparameter *axial-mates*
  '((:pin . :pin-hole) (:axle . :pin-hole) (:axle . :axle-hole))
  "Pairs of connector kinds that join along a shared axis.  An axle in a
round hole turns freely; it is still held.")

(defun complementary-p (a b)
  (let ((ka (connector-kind a)) (kb (connector-kind b)))
    (cond ((or (and (eql ka :stud) (eql kb :socket)) (and (eql ka :socket) (eql kb :stud))) :face)
          ((or (member (cons ka kb) *axial-mates* :test #'equal)
               (member (cons kb ka) *axial-mates* :test #'equal))
           :axial))))

(defun mate-p (a b)
  "The kind of joint connectors a and b make where they stand, or nil."
  (case (complementary-p a b)
    (:face (and (equal (connector-point a) (connector-point b))
                (zero-vector-p (v+ (connector-dir a) (connector-dir b)))
                :face))
    (:axial (let ((d (v- (connector-point b) (connector-point a)))
                  (dir (connector-dir a)))
              (and (zero-vector-p (cross dir (connector-dir b)))
                   (zero-vector-p (cross d dir))
                   (< (abs (dot d dir)) (+ (connector-half a) (connector-half b)))
                   :axial)))))

(defun joints (spec-a spec-b)
  "List of (kind point) for every pair of connectors of the two parts that mate."
  (let ((cb (world-connectors spec-b)))
    (mapcan #'(lambda (a)
                (mapcan #'(lambda (b)
                            (let ((kind (mate-p a b)))
                              (when kind (list (list kind (connector-point a))))))
                        cb))
            (world-connectors spec-a))))

(defun connections (specs)
  "List of (i j joint-count kinds) for every pair of parts that hold each other."
  (collect-pairs #'(lambda (i j a b)
                     (let ((js (joints a b)))
                       (when js (list i j (length js) (remove-duplicates (mapcar #'first js))))))
                 specs))

(defun extents-overlap-p (ea eb)
  "True when the two boxes share volume, not merely a face."
  (every #'(lambda (alo ahi blo bhi) (and (< alo bhi) (< blo ahi)))
         (first ea) (second ea) (first eb) (second eb)))

(defun collide-p (spec-a spec-b)
  "True when the parts take the same space and are not a pin or axle in its hole."
  (and (extents-overlap-p (world-extent spec-a) (world-extent spec-b))
       (notany #'(lambda (j) (eql (first j) :axial)) (joints spec-a spec-b))))

(defun collisions (specs)
  (collect-pairs #'(lambda (i j a b) (when (collide-p a b) (list i j))) specs))

(defun groups (n links)
  "List of lists of part indices, each a set of parts held together."
  (let ((parent (make-array n)) (table (make-hash-table)) result)
    (dotimes (i n) (setf (aref parent i) i))
    (labels ((root (i) (if (= (aref parent i) i) i (setf (aref parent i) (root (aref parent i))))))
      (dolist (link links) (setf (aref parent (root (first link))) (root (second link))))
      (dotimes (i n) (push i (gethash (root i) table))))
    (maphash #'(lambda (root members) (declare (ignore root)) (push (sort members #'<) result)) table)
    (sort result #'< :key #'first)))

;;
;; Snapping: a part dropped near where it would fit is pulled onto the
;; nearest joint that leaves it clear of every other part.
;;

(defun axial-move (a b)
  "The move bringing connector a onto b's axis.  Along the axis it slides
as little as it can in half-stud steps while the shorter of the two
stays wholly on the longer: a pin seats in its hole, a beam rides
anywhere along an axle."
  (let* ((dir (connector-dir a))
         (d (v- (connector-point b) (connector-point a)))
         (along (dot d dir))
         (slack (* 10 (floor (abs (- (connector-half a) (connector-half b))) 10)))
         (offset (max (- slack) (min slack (* 10 (round along 10))))))
    (v- d (mapcar #'(lambda (x) (* offset x)) dir))))

(defun snap-moves (spec others)
  "List of (distance . move) for every way one of the spec's connectors
could be brought onto a mating connector of the others, nearest first."
  (let ((theirs (mapcan #'world-connectors others)) result)
    (dolist (a (world-connectors spec))
      (dolist (b theirs)
        (let ((move (case (complementary-p a b)
                      (:face (when (zero-vector-p (v+ (connector-dir a) (connector-dir b)))
                               (v- (connector-point b) (connector-point a))))
                      (:axial (when (zero-vector-p (cross (connector-dir a) (connector-dir b)))
                                (axial-move a b))))))
          (when move (push (cons (sqrt (dot move move)) move) result)))))
    (stable-sort (nreverse result) #'< :key #'first)))

(defun snap-spec (spec others &key (radius *snap-radius*))
  "The spec moved onto its nearest clear joint with the others within
radius LDU, or nil.  Its :at is rounded to whole LDU first."
  (let ((spec (move-spec spec (mapcar #'round (spec-at spec)))))
    (dolist (candidate (snap-moves spec others))
      (when (> (first candidate) radius) (return nil))
      (let ((moved (move-spec spec (v+ (spec-at spec) (rest candidate)))))
        (unless (some #'(lambda (o) (collide-p moved o)) others)
          (return moved))))))
