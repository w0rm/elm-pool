module LineSegment3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Frame3d
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d
import Point3d
import Point3d.Projection as Point3d
import Quantity exposing (zero)
import Rectangle2d exposing (Rectangle2d)


{-| Project a line segment from 3D world to 2D screen coordinates. Equivalent
to calling [`Point3d.toScreenSpace`](Point3d-Projection#toScreenSpace) on the
two endpoints of the line segment.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> LineSegment3d worldUnits worldCoordinates
    -> LineSegment2d screenUnits screenCoordinates
toScreenSpace camera screen lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    LineSegment2d.from
        (Point3d.toScreenSpace camera screen p1)
        (Point3d.toScreenSpace camera screen p2)
