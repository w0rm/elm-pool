# elm-3d-camera

Copied from https://github.com/ianmackenzie/elm-3d-camera/releases/tag/3.1.0.

This package provides convenient ways to define and use perspective and
orthographic cameras in 3D. It is based on [`elm-geometry`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest)
and has two main goals:

- Provide a way to construct [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/)
  model/view/projection matrices that is more intuitive than using
  [`elm-explorations/linear-algebra`](http://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest)
  directly
- Provide standalone 3D-to-2D projection functionality that can be used
  outside of WebGL

## Defining cameras

To define a camera, you first create a viewpoint which represents the position
and orientation of the camera:

```elm
import Viewpoint3d
import Point3d
import Direction3d

cameraViewpoint =
    Viewpoint3d.lookAt
        { eyePoint = Point3d.meters 10 0 5
        , focalPoint = Point3d.origin
        , upDirection = Direction3d.positiveZ
        }
```

You can then create either a perspective or orthographic camera from the
viewpoint:

```elm
import Camera3d
import Angle
import Length

perspectiveCamera =
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees 30
        }

orthographicCamera =
    Camera3d.orthographic
        { viewpoint = cameraViewpoint
        , viewportHeight = Length.meters 5
        }
```

## WebGL rendering

Once you have a camera, you can use it to get WebGL view and projection
matrices:

```elm
import WebGL.Matrices as WebGL

viewMatrix =
    WebGL.viewMatrix camera

projectionMatrix =
    WebGL.projectionMatrix camera
        { nearClipDepth = Length.meters 0.1
        , farClipDepth = Length.meters 100
        , aspectRatio = 16 / 9
        }
```

## Projection to screen space

You can also use a `Camera3d` to project points, lines, triangles and polylines
from 3D to 2D:

```elm
import Point3d.Projection as Point3d
import LineSegment3d.Projection as LineSegment3d

screen =
    Rectangle2d.from Point2d.origin
        (Point2d.pixels 800 600)

point2d =
    point3d |> Point3d.toScreenSpace camera screen

lineSegment2d =
    lineSegment3d
        |> LineSegment3d.toScreenSpace camera screen
```

This allows you to, for example, do a perspective projection of 3D points and
lines into 2D so that those points and lines can be rendered with SVG (taking
advantage of SVG features like perfect circles and dashed lines which are
difficult to do with WebGL):

![Perspective projection](https://ianmackenzie.github.io/elm-3d-camera/1.0.0/projection.png)

## Roadmap

A few more features are planned:

- More 3D-to-2D projections (directions, axes)
- Construction of 3D cut planes from 2D on-screen lines

## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-3d-camera/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using `elm-3d-camera`, try:

- Joining the **#geometry** or **#webgl** channels on the [Elm Slack](http://elmlang.herokuapp.com/),
  or sending me (**@ianmackenzie**) a message - even if you don't have any
  particular questions right now, it would be great to know what you're hoping
  to do with the package!
- Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
