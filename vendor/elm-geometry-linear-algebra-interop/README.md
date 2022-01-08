# elm-geometry-linear-algebra-interop

Copied from https://github.com/ianmackenzie/elm-geometry-linear-algebra-interop/releases/tag/2.0.2.

This package supports interop between [`ianmackenzie/elm-geometry`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest)
and [`elm-explorations/linear-algebra`](http://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest).
You can:

- Convert `elm-geometry` `Point2d`, `Point3d`, `Vector2d`, `Vector3d`,
  `Direction2d` and `Direction3d` values to and from `linear-algebra` `Vec2`,
  `Vec3` and `Vec4` values
- Convert `elm-geometry` `Frame3d` values to the equivalent `linear-algebra`
  `Mat4` transformation matrices
- Transform `elm-geometry` `Point3d` and `Vector3d` values using
  `linear-algebra` `Mat4` transformation matrices

This is important for working with WebGL, since the [`elm-explorations/webgl`](http://package.elm-lang.org/packages/elm-explorations/webgl/latest)
package requires using `linear-algebra` types when defining meshes and shaders.
This package may also be useful when using other packages that accept or return
`linear-algebra` types. However, you shouldn't need this package for general
use - you should be able to do most geometric transformations you need
(rotations, translations etc.) using `elm-geometry` itself.

## Installation

```
elm install ianmackenzie/elm-geometry-linear-algebra-interop
```

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry-linear-algebra-interop/1.0.2)
is available.

## Usage details

The modules in this package are all designed to be imported using `as` to
'merge' them with the base `elm-geometry` modules; for example, using

```elm
import Point3d exposing (Point3d)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
```

will let you use functions from both modules as if they were part of one big
`Point3d` module. For example, you could use the `toVec3` function from this
package's `Point3d` module with the `origin` value from the base `Point3d`
module as if they were part of the same module:

```elm
Point3d.toVec3 Point3d.origin
--> Math.Vector3.vec3 0 0 0
```

## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-geometry-linear-algebra-interop/issues)
if you run into a bug, if any documentation is missing/incorrect/confusing, or
if there's a new feature that you would find useful. For general questions about
using this package, try:

- Joining the **#geometry** channel on the [Elm Slack](http://elmlang.herokuapp.com/),
  or sending me (**@ianmackenzie**) a message - even if you don't have any
  particular questions right now, it would be great to know what you're hoping
  to do with the package!
- Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums
- Or if you happen to be in the New York area, come on out to the
  [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
