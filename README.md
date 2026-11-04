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

## The editor

Loading the system publishes `/brickyard` on the Gendl web server.  It
opens a new build (a small sample chassis; `?new=empty` for none) at
its own address, `/brickyard?b=<id>`, and keeps it in
`/projects/.state/brickyard-<id>.sexp`, so the address keeps working
across restarts.

- Click a part in the bin to add it: onto the selected part when it
  fits there (in whichever orientation needs the least moving), off to
  the side otherwise.
- Drag a part to slide it across the build; on release it drops onto
  the nearest place it fits, landing on top before going underneath.
- A part moves with what it carries: whatever rests on it and nothing
  else (a plate bridging it and a neighbor stays), and whatever hangs
  from those (a beam on pins, a wheel on an axle) without standing on
  the ground through the rest of the build.  The carried parts glow
  softly when the part is selected, and the group drops, snaps and
  turns as one body.  Hold Shift to move the part alone.
- Arrow keys nudge the selected part a stud, PgUp and PgDn a plate; R,
  T and E turn, tilt and roll it a quarter; C copies it on top; Delete
  removes it; a swatch recolors it.
- The report counts joined pairs and names overlapping and loose
  parts, and keeps the parts list.

"Export LDraw" downloads the build as an LDraw multi-part document
(`.mpd`) for LDraw programs such as LeoCAD.  Parts the official LDraw
library has are placed by their library numbers (3001, 3702, 32316,
3705, 3647 ...), each in the frame its library file uses, checked
against the files' own studs, holes, axle ends and outlines; a brick
several bricks high goes out as a stack of standard bricks.  Wheels,
the motor, the controller and sizes the library lacks go in the same
file as subfiles drawn from the Gendl model.  `to-ldraw` gives the
same text from Lisp.

"Import LDraw" takes an `.ldr` or `.mpd` file and opens it as a new
build.  Library parts Brickyard has come in by number (older numbers
of the same parts too, such as 3023 and 3648), Brickyard's own
subfiles by name, and submodels are flattened through their
placements.  Parts Brickyard does not have, parts turned off the
square grid, and lines that cannot be read are left out and named in
a note.  A file is limited to 4 MB and 2000 parts; numbers are read by
hand, never by the Lisp reader.  `from-ldraw` does the same from Lisp
and returns the specs and that report.

For a private beta, load Gendl's `invite-gate` before Brickyard and
close the gate to a roster kept on the server:

```lisp
(brickyard:private-beta "/path/to/brickyard-invites.sexp" "https://example.com")
(invite-gate:mint-invite! (brickyard:gate) "A tester")  ; => token, link to hand out
```

The link sets a cookie and opens the editor; every page and call under
`/brickyard` then answers only a live invite (the static script and
style stay open).  Without a roster the gate is open, and without
`invite-gate` there is none.

The page is X_ITE, an X3D browser: each part is its own Transform with
a PlaneSensor, so dragging runs in the browser and only the release
goes to the server, which answers with the parts that changed.

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
| `:wheel` | `(d [w])`: d studs across, w wide (2 by default) | its middle |
| `:motor` | none: 2 by 4 studs, two bricks high, output hub at its rear | center of the bottom face |
| `:controller` | none: 4 by 6 studs, two bricks high | center of the bottom face |
| `:gear` | `(teeth)`: 8, 16, 24, 40 in the bin, any count works | its middle |
| `:pin` | `([2 or 3])`: studs long, 2 by default | its middle |
| `:half-pin` | none: a pin on one side of the collar, a stud on the other | its collar |
| `:axle-pin` | `([1 or 2])`: studs of pin; one stud of axle | where the library has it |
| `:bush` | none: a stud long, on an axle | its middle |
| `:cross-block` | `([2 or 3])`: an axle hole across it, pin holes through it | where the library has it |
| `:l-beam` | `(3 5)` or `(2 4)` (its end hole an axle hole) | the end hole of the long arm |
| `:t-beam` | none: 3 by 3 | the end hole of the stem |
| `:angle-connector` | `([1, 2, 3 or 5])`: one arm, two in line, or two at 157.5 (#3) or 112.5 degrees (#5) | the hub |
| `:axle-joiner` | `([2 or 3])` studs long | its middle |
| `:pin-joiner`, `:stop-pin`, `:hole-pin` | none | as the library has them |
| `:pin-block`, `:axle-block` | none: the 1 by 3 cross blocks with four pins, and axle/pin/axle | the middle hole |
| `:round-tile`, `:round-plate` | none: 1 by 1 | center of the bottom face |
| `:rim`, `:tyre` | none: rim 20 by 30, tyre 48 by 20; the tyre seats on the rim | the center |
| `:bent-beam` | none: the 4 by 6 liftarm bent 53.13 degrees (6629) | its end axle hole |
| `:h-beam`, `:axle-beam` | none: the 5 by 3 H (14720); a beam 2 with an axle hole (60483) | as the library has them |
| `:split-cross-block`, `:twin-cross-block`, `:triple-cross-block` | none: 92907, 41678 and 63869 | as the library has them |
| `:axle-hub`, `:axle-connector`, `:cross-pin` | none: two axles at 90 degrees on a pin hole (10197); axle/bush connector (32039); a pin with a pin hole (15100) | as the library has them |
| `:stop-axle` | `([4 or 8])`: studs long | its middle |
| `:hole-axle` | `([2 or 3])`: an axle with a pin hole across its end or middle | the pin hole |
| `:stud-axle`, `:pin-axle` | none: an axle 3 with a stud (6587); an axle 2 with a pin (18651) | as the library has them |
| `:cheese-slope` | none: the 1 by 1 slope two plates high (54200) | center of the bottom face |

The Technic connectors, beams, gears 12 and 20 (double bevel), the
round plate and the rim and tyre follow the LDraw library's own parts:
their pins, holes, studs and axle ends were placed from the library
files and checked against them in every orientation.  A rim turns with
its axle like a wheel, and a tyre seated on it is what the build
stands on.  Parts may run into each other by up to 2 LDU (0.8 mm)
without counting as an overlap: real parts are chamfered where
Brickyard's boxes are square.

The wheel, motor and controller are this program's own designs, sized
to the stud grid.  A wheel has an axle hole through its hub; a motor
has studs on top, sockets below, pin holes across it and an output hub
that holds an axle; a controller has studs on its two end rows, a
screen, four ports and pin holes down both long sides.  `?new=robot`
opens a small drive base.

A gear's pitch radius is half a millimeter per tooth, so gears whose
teeth add up to 32 mesh two studs apart (8 and 24, 16 and 16) and 24
and 40 mesh four apart.  Two gears mesh when their axes are parallel,
their faces overlap and their centers stand exactly the two radii
apart, in any direction across the axes.  The build follows power from
each motor's output: into the axle it holds, into the gears and wheels
on that axle, across each mesh (the driven gear turns the other way,
at the driver's teeth over its own), and on through their axles.  It
reports each wheel a motor turns, how fast against the motor and
which way, and calls a train jammed when one motor would turn a part
at two speeds.  `?new=gearbox` opens an 8-to-24 reduction: the wheel
turns at a third of the motor's speed, reversed.  An axle in a round
pin hole turns freely and carries nothing.

Positions are whole LDraw units (LDU): a stud pitch is 20, a plate 8,
a brick 24; one LDU is 0.4 mm.  Axes follow Gendl: X `:right`, Y
`:rear`, Z `:top`.  `:up` and `:rear` say where the part's own top and
rear face, as axis keywords, which gives the 24 square-on rotations.

A part may instead stand at any angle: `:matrix`, three rows of a
rotation, in place of `:up` and `:rear`, and an `:at` that need not be
whole -- a steered wheel, a roll-cage strut, anything read from another
program at an angle.  Square parts compare exactly; joints with an
angled part are judged within half an LDU and a hundredth of a radian,
and its boxes are oriented with it (overlap by the separating-axis
test), allowed 3.5 LDU against 2 for square parts, since round parts
are boxed square.  Angled parts move, turn a quarter, export and import
like any other.

## How it is built

- `source/specs.lisp` is plain Lisp over the specs: connectors (studs,
  sockets, pin holes, pins, axles), extents, mates, collisions, the
  groups of parts held together, and snapping.  On the square grid all
  of it is exact arithmetic; only angled parts and the bent arms of
  angle connectors #3 and #5 carry floating point.
- `source/parts.lisp` draws each part with Gendl primitives, its studs
  and holes taken from the same connectors the rules use.
- `source/assembly.lisp` is the build: its parts, what holds them, and
  the functions that add, move, snap and remove parts.

## License

GNU Affero General Public License, version 3 or later; see `LICENSE`.
