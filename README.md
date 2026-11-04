# Brickyard

Assemble studded bricks and Technic beams, pins and axles in 3D, in
[Gendl](https://gitlab.common-lisp.net/gendl/gendl).  A build is a list
of part specs; Brickyard works out which parts hold each other, which
collide, which are loose, and pulls a dropped part onto the nearest
place it fits.

```lisp
(asdf:load-system :brickyard)

(defparameter *build* (make-object 'brickyard:assembly))

(the-object *build* connections)        ; ((0 2 8 (:face)) (0 4 1 (:axial)) ...)
(the-object *build* collisions)         ; nil
(the-object *build* loose)              ; nil
(the-object *build* bill-of-materials)  ; ((2 :technic-brick (1 8 3) :grey) ...)

;; drop a 2x2 brick roughly over the plate; it lands on the studs
(let ((i (the-object *build* (add-part! '(:type :brick :size (2 2) :color :yellow)))))
  (the-object *build* (snap-part! i '(17 45 40))))
;; => (:type :brick :size (2 2) :color :yellow :at (20 40 32))
```

## Part specs

```lisp
(:type :brick :size (2 4) :at (0 0 0) :up :top :rear :rear :color :red)
```

| `:type` | `:size` | anchor (`:at`) |
|---|---|---|
| `:brick` | `(w l [plates-high])`, three plates high by default | center of the bottom face |
| `:plate`, `:tile` | `(w l)` | center of the bottom face |
| `:technic-brick` | `(l)`: 1 by l, with l-1 holes | center of the bottom face |
| `:beam` | `(holes)`: studless | the middle hole |
| `:axle` | `(studs-long)` | its middle |
| `:pin` | none: two studs long | its collar |

Positions are whole LDraw units (LDU): a stud pitch is 20, a plate 8,
a brick 24; one LDU is 0.4 mm.  Axes follow Gendl: X `:right`, Y
`:rear`, Z `:top`.  `:up` and `:rear` say where the part's own top and
rear face, as axis keywords, which gives the 24 square-on rotations.

## How it is built

- `source/specs.lisp` is plain Lisp over the specs: connectors (studs,
  sockets, pin holes, pins, axles), extents, mates, collisions, the
  groups of parts held together, and snapping.  All of it is exact
  integer arithmetic.
- `source/parts.lisp` draws each part with Gendl primitives, its studs
  and holes taken from the same connectors the rules use.
- `source/assembly.lisp` is the build: its parts, what holds them, and
  the functions that add, move, snap and remove parts.

## License

GNU Affero General Public License, version 3 or later; see `LICENSE`.
