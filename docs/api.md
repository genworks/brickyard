# The Brickyard API

Flat functions in the package `brickyard-api`, on the current build.
Arguments are literals: numbers, keywords, `t`, `nil`, and lists as
data.  Results are plists and strings.  A refusal is an error whose
message says why; nothing has changed when one comes back.

From Lisp, choose a build first, and quote list arguments as usual:

```lisp
(brickyard-api:use-build (brickyard-api:new-build))
(brickyard-api:add-part :brick :size '(2 4) :at '(0 0 0))
```

## Space

Positions are whole LDraw units (LDU): a stud pitch is 20, a plate 8
high, a brick 24 high.  X is right, Y is rear, Z is up, and the ground
is Z = 0.  `:at` is a part's anchor:

| part | anchor |
|---|---|
| brick, plate, tile, technic-brick, motor, controller | center of the bottom face |
| beam | its middle hole |
| axle, gear, wheel, rim, tyre, pin | its middle |

Studs lie on a grid 20 apart, at odd multiples of 10.  A part an even
number of studs wide stands on a multiple of 20; a part an odd number
of studs wide stands on 10, 30, ...  A brick goes on another at the same
grid, 24 higher; a plate 8 higher.  A technic-brick's holes run through
it side to side, 14 above its bottom, 20 apart, between its studs: an
8-long one at the origin has holes at Y = -60, -40 ... 60.

`:up` and `:rear` say where a part's own top and rear face, as `:top`
`:bottom` `:left` `:right` `:front` `:rear`, at right angles.  An axle,
gear or wheel runs along its own rear: `:rear :right` lays one along X.

## Reading the build

- `(describe-build)`: the build in plain words, what it comes to and
  one line to a part.
- `(parts)`: every part as a plist of `:id :type :size :at :up :rear :color`.
- `(report)`: `:parts :joined :collisions :loose :meshes :drives :jammed`.
  `:collisions` are pairs of ids that overlap, `:loose` the ids held to
  nothing, `:drives` what each motor turns and at what speed against
  it (negative: the other way).  A good build has no collisions and
  nothing loose.
- `(joints id)`: `(other-id kinds)` for each part joined to part `id`.

## Changing the build

- `(add-part type &key size color on near at up rear)` answers the new
  part with its `:id` and `:joined`.
  - `:on id` joins it to that part, anywhere along it, at the joint
    nearest the point `:near` (the top of that part by default), turning
    it to fit unless `:up` or `:rear` is given.  Studs mate with sockets
    and an axle or pin with a hole, so a brick goes on a brick, and an
    axle `:on` a technic-brick goes through its hole.  On an axle a
    part sits wholly on the axle; to overhang the end, use `:at`.
  - `:at (x y z)` places it exactly, with `:up` and `:rear`; it joins
    whatever it meets there, and is refused if it overlaps a part.
  - With neither, it goes off to the side of the build.
- `(move-part id dx dy dz &key alone)` moves the part and what it
  carries, or with `:alone t` the part only.
- `(turn-part id &key about alone)` turns a quarter about the world
  axis `:x`, `:y` or `:z` (the default).
- `(swing-part id degrees &key alone)` swings it about the pin or axle
  it hangs on.  A part held on two is locked, and refused.
- `(color-part id color)`, `(copy-part id)`, `(remove-part id)`,
  `(clear-build)`.

A part carries what rests on it and what hangs from that.  Gears mesh
when their axes are parallel, their faces overlap and their centers
stand half a millimeter per tooth apart in all: teeth that add up to 32
mesh two studs (40 LDU) apart.

## Recipes

A car: two technic-bricks tied by a plate, two axles through them, four
wheels, and a red brick on top.

```lisp
(add-part :technic-brick :size (8) :at (-30 0 0))          ; id 0
(add-part :technic-brick :size (8) :at (30 0 0))           ; id 1
(add-part :plate :size (4 8) :on 0 :near (0 0 24))         ; id 2
(add-part :axle :size (10) :on 0 :near (0 -60 14))         ; id 3
(add-part :axle :size (10) :on 0 :near (0 60 14))          ; id 4
(add-part :wheel :size (5 2) :on 3 :near (90 -60 14))
(add-part :wheel :size (5 2) :on 3 :near (-90 -60 14))
(add-part :wheel :size (5 2) :on 4 :near (90 60 14))
(add-part :wheel :size (5 2) :on 4 :near (-90 60 14))
(add-part :brick :size (2 4) :color :red :on 2 :near (0 0 32))
```

A reduction: a motor turns an 8-tooth gear that turns a 24-tooth gear
two studs away, which carries a wheel round at a third of the speed,
the other way.

```lisp
(add-part :technic-brick :size (8) :at (-30 0 0))          ; id 0
(add-part :technic-brick :size (8) :at (30 0 0))           ; id 1
(add-part :motor :at (100 -20 -10) :rear :left)            ; id 2
(add-part :axle :size (5) :on 0 :near (0 -20 14))          ; id 3
(add-part :axle :size (8) :on 0 :near (0 20 14))           ; id 4
(add-part :gear :size (8) :on 3 :near (0 -20 14))
(add-part :gear :size (24) :color :tan :on 4 :near (0 20 14))
(add-part :wheel :size (5 2) :at (-70 20 14) :rear :right)
```

A wall, a brick at a time, each course offset by a stud:

```lisp
(add-part :brick :size (2 4) :at (0 0 0))
(add-part :brick :size (2 4) :at (0 80 0))
(add-part :brick :size (2 4) :at (0 40 24))
```
