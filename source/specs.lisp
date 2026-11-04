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
;;         :technic-brick (l), :beam (holes), :axle (studs long), :pin (),
;;         :wheel (d [w, default 2]), :motor (), :controller (), :gear (teeth),
;;         :pin ([2 or 3 studs long]), :half-pin (), :axle-pin ([pin 1 or 2]),
;;         :bush (), :cross-block ([2 or 3]), :l-beam (3 5), :round-tile ()
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
    :beam :grey-light :axle :black :pin :black
    :wheel :grey-light :motor :grey-light :controller :white :gear :grey
    :half-pin :grey :axle-pin :blue :bush :grey :cross-block :grey :l-beam :grey-light :round-tile :white
    :angle-connector :grey :axle-joiner :grey :pin-joiner :grey :stop-pin :grey :hole-pin :black
    :pin-block :grey :axle-block :grey :t-beam :grey-light :round-plate :white :rim :grey-light :tyre :black))

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
      ((:beam :axle :gear) (list (first size)))
      (:wheel (list (first size) (or (second size) 2)))
      (:pin (list (or (first size) 2)))
      (:axle-pin (list (or (first size) 1)))
      (:cross-block (list (or (first size) 2)))
      (:l-beam (if (equal size '(2 4)) (list 2 4) (list 3 5)))
      (:angle-connector (list (or (first size) 1)))
      (:axle-joiner (list (or (first size) 3)))
      ((:half-pin :bush :round-tile :motor :controller :pin-joiner :stop-pin :hole-pin
        :pin-block :axle-block :t-beam :round-plate :rim :tyre)
       nil))))

(defun move-spec (spec at)
  (let ((new (copy-list spec))) (setf (getf new :at) at) new))

;;
;; Connectors: where a part joins another.  :stud and :socket mate
;; face to face; :pin, :axle and the holes mate along a shared axis.
;; half is the connector's half-length along that axis.
;;

(defstruct connector kind point dir (half 0) (radius 0))

(defun pitch-radius (teeth)
  "LDU. A gear's pitch radius: half a millimeter per tooth, so gears whose
teeth add up to 32 mesh two studs apart (8 and 24, 16 and 16)."
  (* 5/4 teeth))

(defun gear-half (teeth)
  "LDU. Half a gear's tooth width, as the LDraw library draws them: the
24 and 40 tooth spur gears are half a stud, the rest (8, 16, and the 12
and 20 tooth double bevels) a stud.  Every gear's hub is a stud wide."
  (if (member teeth '(24 40)) 5 10))

(defun row-offset (j n)
  "LDU from the middle of a row of n places, 20 apart, to place j."
  (- (* 20 j) (* 10 (1- n))))

(defun grid-connectors (kind w l z dir &key (rows (iota l)))
  "List of connectors of kind on a w by l stud grid at height z, in the given rows only."
  (let (result)
    (dotimes (i w (nreverse result))
      (dolist (j rows)
        (push (make-connector :kind kind :point (list (row-offset i w) (row-offset j l) z) :dir dir)
              result)))))

(defun hole-row (kind x ys z dir half)
  (mapcar #'(lambda (y) (make-connector :kind kind :point (list x y z) :dir dir :half half)) ys))

;;
;; The robot parts are this program's own, sized to the stud grid:
;;
;; :wheel (d w)   a tire d studs across and w wide on a hub with an axle
;;                hole along its own :rear; the anchor is its middle
;; :motor         a body 2 by 4 studs and two bricks high, studs on top,
;;                sockets below, three pin holes across it, and an output
;;                hub one stud proud of its :rear end that takes an axle
;; :controller    a body 4 by 6 studs and two bricks high, sockets below,
;;                studs on its two end rows, a screen and four ports, and
;;                a row of pin holes into each long side
;;

(defun local-connectors (spec)
  (let ((size (spec-size spec)) (type (spec-type spec)))
    (ecase type
      ((:brick :plate :tile :technic-brick)
       (destructuring-bind (w l h) size
         (append (unless (eql type :tile) (grid-connectors :stud w l (* 8 h) '(0 0 1)))
                 (grid-connectors :socket w l 0 '(0 0 -1))
                 (when (eql type :technic-brick)
                   (hole-row :pin-hole 0 (mapcar #'(lambda (j) (row-offset j (1- l))) (iota (1- l)))
                             14 '(1 0 0) 10)))))
      (:beam
       (let ((n (first size)))
         (hole-row :pin-hole 0 (mapcar #'(lambda (j) (row-offset j n)) (iota n)) 0 '(1 0 0) 10)))
      (:axle
       (list (make-connector :kind :axle :point '(0 0 0) :dir '(0 1 0) :half (* 10 (first size)))))
      ;; pins and axle-pins: a pin connector per stud of pin, along :rear
      (:pin
       (hole-row :pin 0 (if (eql (first size) 3) '(-20 0 20) '(-10 10)) 0 '(0 1 0) 10))
      (:half-pin
       (list (make-connector :kind :pin :point '(0 -10 0) :dir '(0 1 0) :half 10)
             (make-connector :kind :stud :point '(0 0 0) :dir '(0 1 0))))
      (:axle-pin
       (if (eql (first size) 2)
           (append (hole-row :pin 0 '(-20 0) 0 '(0 1 0) 10)
                   (list (make-connector :kind :axle :point '(0 20 0) :dir '(0 1 0) :half 10)))
           (list (make-connector :kind :pin :point '(0 -10 0) :dir '(0 1 0) :half 10)
                 (make-connector :kind :axle :point '(0 10 0) :dir '(0 1 0) :half 10))))
      (:bush
       (list (make-connector :kind :axle-hole :point '(0 0 0) :dir '(0 1 0) :half 10)))
      ;; a cross block lies along :rear: its axle hole across it along
      ;; :right, its pin holes through it along :top
      (:cross-block
       (if (eql (first size) 3)
           (cons (make-connector :kind :axle-hole :point '(0 -20 0) :dir '(1 0 0) :half 10)
                 (hole-row :pin-hole 0 '(0 20) 0 '(0 0 1) 10))
           (list (make-connector :kind :axle-hole :point '(0 0 0) :dir '(1 0 0) :half 10)
                 (make-connector :kind :pin-hole :point '(0 20 0) :dir '(0 0 1) :half 10))))
      ;; the L-beam, as the library has it: holes through along :rear, the
      ;; long arm's end hole at the anchor, five holes up along :top to the
      ;; corner, and from the corner three along :right
      ;; (the 2 by 4's end hole is an axle hole)
      (:l-beam
       (if (equal size '(2 4))
           (cons (make-connector :kind :axle-hole :point '(0 0 0) :dir '(0 1 0) :half 10)
                 (mapcar #'(lambda (p) (make-connector :kind :pin-hole :point p :dir '(0 1 0) :half 10))
                         '((0 0 20) (0 0 40) (0 0 60) (20 0 60))))
           (mapcar #'(lambda (p) (make-connector :kind :pin-hole :point p :dir '(0 1 0) :half 10))
                   '((0 0 0) (0 0 20) (0 0 40) (0 0 60) (0 0 80) (20 0 80) (40 0 80)))))
      ;; the T-beam: a stem of holes up :top from the anchor, a bar of
      ;; three across :right at its head, all through along :rear
      (:t-beam
       (mapcar #'(lambda (p) (make-connector :kind :pin-hole :point p :dir '(0 1 0) :half 10))
               '((0 0 0) (0 0 20) (-20 0 40) (0 0 40) (20 0 40))))
      ;; angle connectors: a pin hole across the hub along :right, an axle
      ;; hole in each arm along :top, one arm down (#1) or one each way (#2)
      (:angle-connector
       (cons (make-connector :kind :pin-hole :point '(0 0 0) :dir '(1 0 0) :half 10)
             (mapcar #'(lambda (z) (make-connector :kind :axle-hole :point (list 0 0 z) :dir '(0 0 1) :half 10))
                     (if (eql (first size) 2) '(-20 20) '(-20)))))
      ;; joiners and the stop and hole pins lie along :rear like pins
      (:axle-joiner
       (hole-row :axle-hole 0 (if (eql (first size) 2) '(-10 10) '(-20 20)) 0 '(0 1 0) 10))
      (:pin-joiner
       (hole-row :pin-hole 0 '(-10 10) 0 '(0 1 0) 10))
      (:stop-pin
       (append (hole-row :pin 0 '(0 20) 0 '(0 1 0) 10)
               (list (make-connector :kind :axle-hole :point '(0 -20 0) :dir '(0 1 0) :half 10))))
      (:hole-pin
       (append (hole-row :pin 0 '(-20 20) 0 '(0 1 0) 10)
               (list (make-connector :kind :pin-hole :point '(0 0 0) :dir '(0 0 1) :half 10))))
      ;; 1 by 3 blocks along :rear: pin holes across at the ends and through
      ;; the middle, four pins standing out up and down (48989); or axle
      ;; holes across at the ends, a pin hole through the middle (32184)
      (:pin-block
       (append (hole-row :pin-hole 0 '(-20 20) 0 '(1 0 0) 10)
               (list (make-connector :kind :pin-hole :point '(0 0 0) :dir '(0 0 1) :half 10))
               (mapcan #'(lambda (y) (mapcar #'(lambda (z) (make-connector :kind :pin :point (list 0 y z) :dir '(0 0 1) :half 10))
                                             '(-20 20)))
                       '(-20 20))))
      (:axle-block
       (append (hole-row :axle-hole 0 '(-20 20) 0 '(1 0 0) 10)
               (list (make-connector :kind :pin-hole :point '(0 0 0) :dir '(0 0 1) :half 10))))
      (:round-plate
       (list (make-connector :kind :stud :point '(0 0 8) :dir '(0 0 1))
             (make-connector :kind :socket :point '(0 0 0) :dir '(0 0 -1))))
      ;; a rim turns on an axle along :rear, its axle hole a stud deep to
      ;; one side; a tyre seats on it, the two sharing a center and an axis
      (:rim
       (list (make-connector :kind :axle-hole :point '(0 -10 0) :dir '(0 1 0) :half 10)
             (make-connector :kind :rim-seat :point '(0 0 0) :dir '(0 1 0) :radius 38)))
      (:tyre
       (list (make-connector :kind :tyre-seat :point '(0 0 0) :dir '(0 1 0) :radius 38)))
      (:round-tile
       (list (make-connector :kind :socket :point '(0 0 0) :dir '(0 0 -1))))
      (:wheel
       (list (make-connector :kind :axle-hole :point '(0 0 0) :dir '(0 1 0) :half (* 10 (second size)))))
      (:gear
       (let ((half (gear-half (first size))))
         (list (make-connector :kind :axle-hole :point '(0 0 0) :dir '(0 1 0) :half 10)
               (make-connector :kind :gear :point '(0 0 0) :dir '(0 1 0) :half half
                               :radius (pitch-radius (first size))))))
      (:motor
       (append (grid-connectors :stud 2 4 48 '(0 0 1))
               (grid-connectors :socket 2 4 0 '(0 0 -1))
               (hole-row :pin-hole 0 '(-20 0 20) 24 '(1 0 0) 20)
               (list (make-connector :kind :axle-hole :point '(0 50 24) :dir '(0 1 0) :half 10))))
      (:controller
       (append (grid-connectors :stud 4 6 48 '(0 0 1) :rows '(0 5))
               (grid-connectors :socket 4 6 0 '(0 0 -1))
               (hole-row :pin-hole -30 (mapcar #'(lambda (j) (row-offset j 6)) (iota 6)) 24 '(1 0 0) 10)
               (hole-row :pin-hole 30 (mapcar #'(lambda (j) (row-offset j 6)) (iota 6)) 24 '(1 0 0) 10))))))

(defun local-extent (spec)
  "List of two corners, low and high, of the part's own box in LDU, studs left out."
  (let ((size (spec-size spec)))
    (ecase (spec-type spec)
      ((:brick :plate :tile :technic-brick)
       (destructuring-bind (w l h) size
         (list (list (* -10 w) (* -10 l) 0) (list (* 10 w) (* 10 l) (* 8 h)))))
      (:beam (let ((y (+ (* 10 (1- (first size))) 9))) (list (list -10 (- y) -9) (list 10 y 9))))
      (:axle (let ((y (* 10 (first size)))) (list (list -6 (- y) -6) (list 6 y 6))))
      (:pin (if (eql (first size) 3) (list '(-6 -30 -6) '(6 30 6)) (list '(-6 -20 -6) '(6 20 6))))
      (:half-pin (list '(-6 -20 -6) '(6 0 6)))
      (:axle-pin (if (eql (first size) 2) (list '(-6 -30 -6) '(6 30 6)) (list '(-6 -20 -6) '(6 20 6))))
      (:bush (list '(-9 -10 -9) '(9 10 9)))
      (:cross-block (if (eql (first size) 3) (list '(-10 -30 -10) '(10 30 10)) (list '(-10 -10 -10) '(10 30 10))))
      (:l-beam (if (equal size '(2 4)) (list '(-9 -10 -9) '(29 10 69)) (list '(-9 -10 -9) '(49 10 89))))
      (:t-beam (list '(-29 -10 -9) '(29 10 49)))
      (:angle-connector (if (eql (first size) 2) (list '(-10 -10 -30) '(10 10 30)) (list '(-10 -10 -30) '(10 10 10))))
      (:axle-joiner (if (eql (first size) 2) (list '(-8 -20 -8) '(8 20 8)) (list '(-8 -30 -8) '(8 30 8))))
      (:pin-joiner (list '(-10 -20 -10) '(10 20 10)))
      (:stop-pin (list '(-9 -30 -9) '(9 30 9)))
      (:hole-pin (list '(-9 -23 -10) '(9 23 10)))
      (:pin-block (list '(-10 -30 -30) '(10 30 30)))
      (:axle-block (list '(-10 -30 -10) '(10 30 10)))
      (:round-plate (list '(-10 -10 0) '(10 10 8)))
      (:rim (list '(-38 -25 -38) '(38 25 38)))
      (:tyre (list '(-62 -25 -62) '(62 25 62)))
      (:round-tile (list '(-10 -10 0) '(10 10 8)))
      (:wheel (destructuring-bind (d w) size
                (list (list (* -10 d) (* -10 w) (* -10 d)) (list (* 10 d) (* 10 w) (* 10 d)))))
      (:gear (let ((r (+ (pitch-radius (first size)) 5/2)))
               (list (list (- r) -10 (- r)) (list r 10 r))))
      (:motor (list '(-20 -40 0) '(20 60 48)))
      (:controller (list '(-40 -60 0) '(40 60 48))))))

(defun world-connectors (spec)
  (let ((rows (part-rotation (spec-up spec) (spec-rear spec))) (at (spec-at spec)))
    (mapcar #'(lambda (c)
                (make-connector :kind (connector-kind c) :half (connector-half c)
                                :radius (connector-radius c)
                                :point (v+ at (rotate rows (connector-point c)))
                                :dir (rotate rows (connector-dir c))))
            (local-connectors spec))))

(defun ring-boxes (inner outer half)
  "Four boxes that bound a ring from radius inner to outer round the part's
:rear axis, half its length each way: a square tunnel down the middle."
  (list (list (list (- outer) (- half) inner) (list outer half outer))
        (list (list (- outer) (- half) (- outer)) (list outer half (- inner)))
        (list (list (- outer) (- half) (- inner)) (list (- inner) half inner))
        (list (list inner (- half) (- inner)) (list outer half inner))))

(defun local-boxes (spec)
  "List of boxes, each two corners, that the part fills: its extent, or for
an L-beam one box per arm, so a part in the crook of the L is clear."
  (case (spec-type spec)
    (:l-beam (if (equal (spec-size spec) '(2 4))
                 (list (list '(-9 -10 -9) '(9 10 69)) (list '(-9 -10 51) '(29 10 69)))
                 (list (list '(-9 -10 -9) '(9 10 89)) (list '(-9 -10 71) '(49 10 89)))))
    (:t-beam (list (list '(-9 -10 -9) '(9 10 49)) (list '(-29 -10 31) '(29 10 49))))
    (:pin-block (list (list '(-10 -30 -10) '(10 30 10))
                      (list '(-6 -26 -30) '(6 -14 30)) (list '(-6 14 -30) '(6 26 30))))
    ;; a rim is a drum round a hub: the hub around its axle hole, and four
    ;; slabs for the drum, leaving a tunnel along the axle for bushes; a
    ;; tyre is the same ring further out, round the rim
    (:rim (cons (list '(-8 -20 -8) '(8 0 8)) (ring-boxes 12 38 25)))
    (:tyre (ring-boxes 38 62 25))
    (t (list (local-extent spec)))))

(defun world-boxes (spec)
  (let ((rows (part-rotation (spec-up spec) (spec-rear spec))) (at (spec-at spec)))
    (mapcar #'(lambda (box)
                (let ((corners (mapcar #'(lambda (p) (rotate rows p)) box)))
                  (list (v+ at (apply #'mapcar #'min corners)) (v+ at (apply #'mapcar #'max corners)))))
            (local-boxes spec))))

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
          ((and (eql ka :gear) (eql kb :gear)) :mesh)
          ((or (and (eql ka :rim-seat) (eql kb :tyre-seat)) (and (eql ka :tyre-seat) (eql kb :rim-seat))) :seat)
          ((or (member (cons ka kb) *axial-mates* :test #'equal)
               (member (cons kb ka) *axial-mates* :test #'equal))
           :axial))))

(defun mate-p (a b)
  "The kind of joint connectors a and b make where they stand, or nil."
  (case (complementary-p a b)
    (:face (and (equal (connector-point a) (connector-point b))
                (zero-vector-p (v+ (connector-dir a) (connector-dir b)))
                :face))
    ;; a tyre on its rim: one center, one axis
    (:seat (and (equal (connector-point a) (connector-point b))
                (zero-vector-p (cross (connector-dir a) (connector-dir b)))
                :seat))
    (:axial (let ((d (v- (connector-point b) (connector-point a)))
                  (dir (connector-dir a)))
              (and (zero-vector-p (cross dir (connector-dir b)))
                   (zero-vector-p (cross d dir))
                   (< (abs (dot d dir)) (+ (connector-half a) (connector-half b)))
                   :axial)))
    ;; two gears on parallel axes, faces overlapping, centers apart by
    ;; the sum of their pitch radii -- in any direction across the axes
    (:mesh (let* ((d (v- (connector-point b) (connector-point a)))
                  (dir (connector-dir a))
                  (along (dot d dir))
                  (across (v- d (mapcar #'(lambda (x) (* along x)) dir)))
                  (reach (+ (connector-radius a) (connector-radius b))))
             (and (zero-vector-p (cross dir (connector-dir b)))
                  (< (abs along) (+ (connector-half a) (connector-half b)))
                  (= (dot across across) (* reach reach))
                  :mesh)))))

;;
;; The survey.  Every question about a build is asked of pairs of parts,
;; and two parts can only join or collide when their boxes touch.  So a
;; part's box and connectors are worked out once, and connectors are
;; compared only for pairs whose boxes touch: a few hundred parts cost
;; milliseconds where comparing every pair connector by connector cost
;; seconds.
;;

(defstruct (placed (:constructor %make-placed)) spec extent boxes connectors)

(defun placed (spec)
  (%make-placed :spec spec :extent (world-extent spec) :boxes (world-boxes spec)
                :connectors (world-connectors spec)))

(defparameter *overlap-slack* 2
  "LDU. Boxes that run into each other by no more than this along some axis
do not collide: real parts are chamfered where these boxes are square, and
models built in other programs sit that close (0.8 mm).")

(defun boxes-overlap-p (pa pb)
  "True when the two parts' boxes share volume, deeper than *overlap-slack*
along every axis: their extents, then each of their boxes."
  (flet ((deep-p (ea eb)
           (every #'(lambda (alo ahi blo bhi) (and (< (+ alo *overlap-slack*) bhi) (< (+ blo *overlap-slack*) ahi)))
                  (first ea) (second ea) (first eb) (second eb))))
    (and (deep-p (placed-extent pa) (placed-extent pb))
         (some #'(lambda (a) (some #'(lambda (b) (deep-p a b)) (placed-boxes pb)))
               (placed-boxes pa)))))

(defun extents-overlap-p (ea eb)
  "True when the two boxes share volume, not merely a face."
  (every #'(lambda (alo ahi blo bhi) (and (< alo bhi) (< blo ahi)))
         (first ea) (second ea) (first eb) (second eb)))

(defun extents-touch-p (ea eb)
  "True when the two boxes share volume or a face, or come within a unit."
  (every #'(lambda (alo ahi blo bhi) (and (<= alo (1+ bhi)) (<= blo (1+ ahi))))
         (first ea) (second ea) (first eb) (second eb)))

(defun placed-joints (pa pb)
  "List of (kind ca cb) for every connector ca of pa and cb of pb that mate."
  (when (extents-touch-p (placed-extent pa) (placed-extent pb))
    (let ((cbs (placed-connectors pb)))
      (mapcan #'(lambda (ca)
                  (mapcan #'(lambda (cb)
                              (let ((kind (mate-p ca cb)))
                                (when kind (list (list kind ca cb)))))
                          cbs))
              (placed-connectors pa)))))

(defun placed-collide-p (pa pb)
  "True when the parts take the same space and are not a pin or axle in
its hole, nor two gears in mesh."
  (and (boxes-overlap-p pa pb)
       (notany #'(lambda (j) (member (first j) '(:axial :mesh :seat))) (placed-joints pa pb))))

(defstruct (survey (:constructor %make-survey)) placed pairs)

(defun survey (specs)
  "A survey of the build: each part placed, and (i j joints) for every
pair i < j whose boxes touch."
  (let* ((placed (coerce (mapcar #'placed specs) 'vector))
         (n (length placed))
         (pairs nil))
    (dotimes (i n)
      (do ((j (1+ i) (1+ j))) ((>= j n))
        (when (extents-touch-p (placed-extent (aref placed i)) (placed-extent (aref placed j)))
          (push (list i j (placed-joints (aref placed i) (aref placed j))) pairs))))
    (%make-survey :placed placed :pairs (nreverse pairs))))

(defun joints (spec-a spec-b)
  "List of (kind point) for every pair of connectors of the two parts that mate."
  (mapcar #'(lambda (j) (list (first j) (connector-point (second j))))
          (placed-joints (placed spec-a) (placed spec-b))))

(defun connections (specs &optional (survey (survey specs)))
  "List of (i j joint-count kinds) for every pair of parts that hold each other."
  (mapcan #'(lambda (pair)
              (destructuring-bind (i j js) pair
                (when js (list (list i j (length js) (remove-duplicates (mapcar #'first js)))))))
          (survey-pairs survey)))

(defun collide-p (spec-a spec-b)
  "True when the parts take the same space and are not a pin or axle in
its hole, nor two gears in mesh."
  (placed-collide-p (placed spec-a) (placed spec-b)))

(defun collisions (specs &optional (survey (survey specs)))
  (mapcan #'(lambda (pair)
              (destructuring-bind (i j js) pair
                (when (and (boxes-overlap-p (aref (survey-placed survey) i) (aref (survey-placed survey) j))
                           (notany #'(lambda (k) (member (first k) '(:axial :mesh :seat))) js))
                  (list (list i j)))))
          (survey-pairs survey)))

;;
;; What a robot build is asked: which motor turns which wheel, and what
;; it stands on.
;;

(defun power-flow (specs &optional (survey (survey specs)))
  "Two values.  A list of (motor wheel ratio): each wheel a motor turns,
and how fast against the motor -- 1/3 is a third the speed, negative
is the other way round.  And a list of the indices of parts that two
paths from one motor would turn at different speeds: a jammed train.

Power goes from a motor's output into the axle it holds; from an axle
into the gears and wheels whose axle holes it fills; from a gear or
wheel into its axle and, for a gear, into the gears it meshes with,
whose speed is the driver's times its teeth over theirs, reversed.
An axle in a round pin hole turns freely and carries nothing."
  (power-flow-of specs survey))

(defun pair-table (survey)
  "Hash table from (i . j), i < j, to the pair's joints, for the pairs that join."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair (survey-pairs survey) table)
      (when (third pair) (setf (gethash (cons (first pair) (second pair)) table) (third pair))))))

(defun neighbors (survey)
  "Vector of lists: for each part, the parts it joins."
  (let ((result (make-array (length (survey-placed survey)) :initial-element nil)))
    (dolist (pair (survey-pairs survey) result)
      (when (third pair)
        (push (second pair) (aref result (first pair)))
        (push (first pair) (aref result (second pair)))))))

(defun held-by-p (table i kind-i j kind-j)
  "True when a connector of kind-i on part i mates one of kind-j on part j."
  (if (< i j)
      (some #'(lambda (k) (and (eql (connector-kind (second k)) kind-i) (eql (connector-kind (third k)) kind-j)))
            (gethash (cons i j) table))
      (some #'(lambda (k) (and (eql (connector-kind (third k)) kind-i) (eql (connector-kind (second k)) kind-j)))
            (gethash (cons j i) table))))

(defparameter *shafts* '(:axle :axle-pin)
  "Part types that carry turning along their axle: an axle-pin's axle end
turns what sits on it as an axle does; its pin end turns freely.")

(defun power-flow-of (specs survey)
  (let* ((n (length specs))
         (types (coerce (mapcar #'spec-type specs) 'vector))
         (table (pair-table survey))
         (near (neighbors survey))
         drives jammed)
    (labels ((held-p (i kind-i j kind-j) (held-by-p table i kind-i j kind-j))
             (teeth (i) (first (spec-size (nth i specs))))
             (onward (i speed)
               "List of (j speed) for the parts part i turns directly."
               (let (result)
                 (dolist (j (aref near i) result)
                   (progn
                     (case (aref types i)
                       ((:axle :axle-pin)
                        (when (and (member (aref types j) '(:gear :wheel :rim)) (held-p j :axle-hole i :axle))
                          (push (list j speed) result)))
                       ((:gear :wheel :rim)
                        (when (and (member (aref types j) *shafts*) (held-p i :axle-hole j :axle))
                          (push (list j speed) result))
                        (when (and (eql (aref types i) :gear) (eql (aref types j) :gear)
                                   (held-p i :gear j :gear))
                          (push (list j (- (* speed (/ (teeth i) (teeth j))))) result)))))))))
      (dotimes (m n)
        (when (eql (aref types m) :motor)
          (let ((speeds (make-hash-table))
                (queue (remove nil (mapcar #'(lambda (j)
                                               (when (and (member (aref types j) *shafts*)
                                                          (held-p m :axle-hole j :axle))
                                                 (list j 1)))
                                           (aref near m)))))
            (do () ((null queue))
              (destructuring-bind (i speed) (pop queue)
                (multiple-value-bind (known seen?) (gethash i speeds)
                  (cond ((not seen?)
                         (setf (gethash i speeds) speed)
                         (setf queue (append queue (onward i speed))))
                        ((/= known speed) (pushnew i jammed))))))
            (dotimes (w n)
              (when (member (aref types w) '(:wheel :rim))
                (multiple-value-bind (speed seen?) (gethash w speeds)
                  (when seen? (push (list m w speed) drives))))))))
      (values (nreverse drives) (sort jammed #'<)))))

(defun ground-contacts (specs &key (tolerance 2))
  "List of indices of the parts whose lowest point is within tolerance LDU of the build's."
  (when specs
    (let* ((lows (mapcar #'(lambda (s) (third (first (world-extent s)))) specs))
           (lowest (reduce #'min lows)))
      (remove nil (mapcar #'(lambda (i z) (when (<= (- z lowest) tolerance) i))
                          (iota (length specs)) lows)))))

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

(defun axial-moves (a b)
  "List of the moves bringing connector a onto b's axis, nearest first.
Along the axis it may stop at any half-stud step that keeps the shorter
of the two wholly on the longer: a pin seats in its hole, a gear or a
beam rides anywhere along an axle, so when the nearest stop is taken
the next one along is offered."
  (let* ((dir (connector-dir a))
         (d (v- (connector-point b) (connector-point a)))
         (along (dot d dir))
         (slack (* 10 (floor (abs (- (connector-half a) (connector-half b))) 10)))
         (offsets (mapcar #'(lambda (k) (- (* 10 k) slack)) (iota (1+ (/ slack 5) )))))
    (mapcar #'(lambda (offset) (v- d (mapcar #'(lambda (x) (* offset x)) dir)))
            (stable-sort (remove-if #'(lambda (o) (> (abs o) slack)) offsets)
                         #'< :key #'(lambda (o) (abs (- o along)))))))

(defun snap-moves (spec others &optional (theirs (mapcan #'world-connectors others)))
  "List of (distance . move) for every way one of the spec's connectors
could be brought onto a mating connector of the others, nearest first.
theirs, the others' connectors, may be given when they are known."
  (let (result)
    (dolist (a (world-connectors spec))
      (dolist (b theirs)
        (dolist (move (case (complementary-p a b)
                        (:face (when (zero-vector-p (v+ (connector-dir a) (connector-dir b)))
                                 (list (v- (connector-point b) (connector-point a)))))
                        (:seat (when (zero-vector-p (cross (connector-dir a) (connector-dir b)))
                                 (list (v- (connector-point b) (connector-point a)))))
                        (:axial (when (zero-vector-p (cross (connector-dir a) (connector-dir b)))
                                  (axial-moves a b)))))
          (push (cons (sqrt (dot move move)) move) result))))
    (stable-sort (nreverse result) #'< :key #'first)))

(defun collides-with-any-p (spec others)
  "True when spec collides with any of others, specs or placed parts."
  (let ((p (placed spec)))
    (some #'(lambda (o) (placed-collide-p p (if (placed-p o) o (placed o)))) others)))

(defun move-rank (move)
  "List compared in order: the plan distance, rising before sinking, then
how far up or down."
  (destructuring-bind (dx dy dz) move
    (list (round (sqrt (+ (* dx dx) (* dy dy)))) (if (minusp dz) 1 0) (abs dz))))

(defun rank< (a b)
  (cond ((null a) nil)
        ((< (first a) (first b)) t)
        ((> (first a) (first b)) nil)
        (t (rank< (rest a) (rest b)))))

(defun shift-specs (specs move)
  (mapcar #'(lambda (s) (move-spec s (v+ (spec-at s) move))) specs))

(defun group-clear-p (group others)
  (notany #'(lambda (s) (collides-with-any-p s others)) group))

(defun drop-group (group others &key (reach 40) (free? t))
  "List. The group of specs, moved as one rigid body, as it lands when let
go where it stands: onto the clear joint nearest in plan within reach
LDU that any member makes with the others, any height, landing on top
before going underneath; failing that, when free? and it is clear where
it is, left there with its first member on a grid of half studs and
plates (it will be loose); else nil."
  (let* ((group (mapcar #'(lambda (s) (move-spec s (mapcar #'round (spec-at s)))) group))
         (others (mapcar #'placed others))
         (theirs (mapcan #'(lambda (o) (copy-list (placed-connectors o))) others))
         (moves (remove-if #'(lambda (m) (> (first (move-rank m)) reach))
                           (mapcan #'(lambda (s) (mapcar #'rest (snap-moves s nil theirs))) group))))
    (or (dolist (move (stable-sort moves #'rank< :key #'move-rank))
          (let ((moved (shift-specs group move)))
            (when (group-clear-p moved others) (return moved))))
        (when free?
          (let* ((lead (spec-at (first group)))
                 (gridded (shift-specs group (v- (destructuring-bind (x y z) lead
                                                   (list (* 10 (round x 10)) (* 10 (round y 10)) (* 8 (round z 8))))
                                                 lead))))
            (when (group-clear-p gridded others) gridded))))))

(defun drop-spec (spec others &rest keys &key reach free?)
  "The spec as drop-group lands it alone, or nil."
  (declare (ignore reach free?))
  (first (apply #'drop-group (list spec) others keys)))

;;
;; Turning: quarter turns about the world's axes, the anchor staying put.
;;

(defun quarter-turn (about v)
  (destructuring-bind (x y z) v
    (ecase about
      (:z (list (- y) x z))
      (:x (list x (- z) y))
      (:y (list z y (- x))))))

(defun turn-spec (spec about)
  (let ((new (copy-list spec)))
    (setf (getf new :up) (axis-key (quarter-turn about (axis (spec-up spec))))
          (getf new :rear) (axis-key (quarter-turn about (axis (spec-rear spec)))))
    new))

(defun turn-group (group about)
  "The group turned a quarter about the world axis about, round its first
member's anchor."
  (let ((pivot (spec-at (first group))))
    (mapcar #'(lambda (s)
                (move-spec (turn-spec s about) (v+ pivot (quarter-turn about (v- (spec-at s) pivot)))))
            group)))

;;
;; What a part carries: what rests on it alone, and what hangs from it.
;;

(defun carried (specs index &optional (survey (survey specs)))
  "List of the indices that move with part index, index first.  A part
comes along when everything it rests on comes along; and a set of parts
held to the moving ones but not standing on the ground through anything
else (a beam on pins, a wheel on an axle in the moving part) comes along
too.  The ground is the lowest of the parts that stay.  A part resting
on the moving part and on something else stays."
  (let* ((n (length specs))
         (moving (make-array n :initial-element nil))
         (supports (make-array n :initial-element nil))
         (links (mapcan #'(lambda (pair) (when (third pair) (list (list (first pair) (second pair)))))
                        (survey-pairs survey)))
         (near (neighbors survey))
         (lows (map 'vector #'(lambda (p) (third (first (placed-extent p)))) (survey-placed survey))))
    ;; who sits on whom, from the face joints: a socket on a stud
    (dolist (pair (survey-pairs survey))
      (destructuring-bind (i j js) pair
        (dolist (k js)
          (when (eql (first k) :face)
            (if (eql (connector-kind (second k)) :socket)
                (pushnew j (aref supports i))
                (pushnew i (aref supports j)))))))
    (setf (aref moving index) t)
    (do ((grown t)) ((not grown))
      (setf grown nil)
      ;; what rests on moving parts alone
      (dotimes (i n)
        (let ((under (aref supports i)))
          (when (and under (not (aref moving i)) (every #'(lambda (j) (aref moving j)) under))
            (setf (aref moving i) t grown t))))
      ;; what hangs from moving parts, off the ground
      (let* ((rest (remove-if #'(lambda (i) (aref moving i)) (iota n)))
             (lowest (and rest (reduce #'min rest :key #'(lambda (i) (aref lows i)))))
             (inner (remove-if #'(lambda (l) (or (aref moving (first l)) (aref moving (second l)))) links)))
        (dolist (group (groups n inner))
          (when (and (notany #'(lambda (i) (aref moving i)) group)
                     (notany #'(lambda (i) (<= (- (aref lows i) lowest) 2)) group)
                     (some #'(lambda (i) (some #'(lambda (j) (aref moving j)) (aref near i))) group))
            (dolist (i group) (setf (aref moving i) t))
            (setf grown t)))))
    (cons index (remove-if-not #'(lambda (i) (and (/= i index) (aref moving i))) (iota n)))))

(defparameter *orientations*
  (let (result)
    (dolist (up '(:top :bottom :right :left :rear :front) (nreverse result))
      (dolist (rear '(:rear :front :right :left :top :bottom))
        (when (zerop (dot (axis up) (axis rear))) (push (list up rear) result)))))
  "List of the 24 (up rear) pairs.")

(defun fit-spec (spec others &key (reach 40))
  "The spec snapped onto the build in whichever of the 24 orientations
needs the shortest move, its own orientation winning a tie; nil when none
snaps within reach."
  (let ((own (list (spec-up spec) (spec-rear spec))) best best-distance)
    (dolist (orientation (cons own (remove own *orientations* :test #'equal)) best)
      (let* ((turned (let ((new (copy-list spec)))
                       (setf (getf new :up) (first orientation) (getf new :rear) (second orientation))
                       new))
             (dropped (drop-spec turned others :reach reach :free? nil)))
        (when dropped
          (let* ((move (v- (spec-at dropped) (spec-at spec)))
                 (distance (sqrt (dot move move))))
            (when (or (null best) (< distance best-distance))
              (setf best dropped best-distance distance))))))))

;;
;; Part ids: an editor names parts by id, which survives the removal of
;; others where an index does not.
;;

(defun spec-id (spec) (getf spec :id))

(defun ensure-ids (specs)
  (let ((next (1+ (reduce #'max specs :key #'(lambda (s) (or (spec-id s) -1)) :initial-value -1))))
    (mapcar #'(lambda (s) (if (spec-id s) s (prog1 (list* :id next s) (incf next)))) specs)))

(defun snap-spec (spec others &key (radius *snap-radius*))
  "The spec moved onto its nearest clear joint with the others within
radius LDU, or nil.  Its :at is rounded to whole LDU first."
  (let ((spec (move-spec spec (mapcar #'round (spec-at spec)))))
    (dolist (candidate (snap-moves spec others))
      (when (> (first candidate) radius) (return nil))
      (let ((moved (move-spec spec (v+ (spec-at spec) (rest candidate)))))
        (unless (some #'(lambda (o) (collide-p moved o)) others)
          (return moved))))))
