module WebGL.Matrices exposing
    ( modelMatrix, viewMatrix, projectionMatrix
    , modelViewMatrix, viewProjectionMatrix, modelViewProjectionMatrix
    )

{-| These matrices can be used for rendering 3D [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/)
scenes. For in-depth explanations of how they are used, check out:

  - <https://learnopengl.com/Getting-started/Coordinate-Systems>
  - <http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/>
  - <http://www.songho.ca/opengl/gl_transform.html>


# Individual matrices

@docs modelMatrix, viewMatrix, projectionMatrix


# Combined matrices

@docs modelViewMatrix, viewProjectionMatrix, modelViewProjectionMatrix

-}

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Math.Matrix4 exposing (Mat4)
import Quantity exposing (Quantity(..))
import Viewpoint3d exposing (Viewpoint3d)


{-| Construct a WebGL model matrix given a `Frame3d` that defines the position
and orientation of an object. Multiplying by this matrix transforms from local
object coordinates (coordinates relative to the given frame) to world
coordinates.

(This is an alias of [`Frame3d.toMat4`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-linear-algebra-interop/latest/Geometry-Interop-LinearAlgebra-Frame3d#toMat4),
provided here for consistency/convenience.)

-}
modelMatrix : Frame3d units coordinates defines -> Mat4
modelMatrix frame =
    Frame3d.toMat4 frame


{-| Construct a WebGL view matrix for a given viewpoint. Multiplying by this
matrix transforms from world coordinates to view (eye) coordinates.

Note that to avoid accuracy/roundoff issues (especially if both the viewpoint
and rendered objects are far from the world origin point), it is often better to
use [`modelViewMatrix`](#modelViewMatrix) instead of calling `modelMatrix` and
`viewMatrix` separately.

-}
viewMatrix : Viewpoint3d units coordinates -> Mat4
viewMatrix camera =
    modelViewMatrix Frame3d.atOrigin camera


{-| Construct a WebGL model-view matrix given a viewpoint and a `Frame3d` that
defines the position and orientation of an object.

Multiplying by this matrix transforms from local object coordinates (coordinates
relative to the given frame) directly to view (eye) coordinates without first
transforming to world coordinates. Avoiding this intermediate conversion to
world coordinates improves accuracy, especially if both the object and camera
are far from the world origin point.

-}
modelViewMatrix : Frame3d units coordinates defines -> Viewpoint3d units coordinates -> Mat4
modelViewMatrix modelFrame (Types.Viewpoint3d viewpointFrame) =
    Frame3d.toMat4 (modelFrame |> Frame3d.relativeTo viewpointFrame)


{-| Construct a WebGL projection matrix for a given camera, by supplying near
and far clip depths as well as the aspect ratio (width over height) of the WebGL
window being rendered to. Multiplying by this matrix converts from view
coordinates to [clip coordinates](https://en.wikipedia.org/wiki/Clip_coordinates).

Using a value of [`Quantity.positiveInfinity`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#positiveInfinity)
for `farClipDepth` is supported, and this often works well for perspective
cameras. However, for orthographic cameras, an infinite far clip depth means
that depth testing will not work. For things like line drawings this might be
fine, but if you are depending on closer objects being reliably drawn in front
of further-away objects (at least when using an orthographic camera) then you
will have to specify a finite far clip depth.

-}
projectionMatrix :
    Camera3d units coordinates
    ->
        { nearClipDepth : Quantity Float units
        , farClipDepth : Quantity Float units
        , aspectRatio : Float
        }
    -> Mat4
projectionMatrix (Types.Camera3d camera) { nearClipDepth, farClipDepth, aspectRatio } =
    let
        (Quantity n) =
            Quantity.abs nearClipDepth

        (Quantity f) =
            Quantity.abs farClipDepth
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            if isInfinite f then
                Math.Matrix4.fromRecord
                    { m11 = 1 / (aspectRatio * frustumSlope)
                    , m21 = 0
                    , m31 = 0
                    , m41 = 0
                    , m12 = 0
                    , m22 = 1 / frustumSlope
                    , m32 = 0
                    , m42 = 0
                    , m13 = 0
                    , m23 = 0
                    , m33 = -1
                    , m43 = -1
                    , m14 = 0
                    , m24 = 0
                    , m34 = -2 * n
                    , m44 = 0
                    }

            else
                Math.Matrix4.fromRecord
                    { m11 = 1 / (aspectRatio * frustumSlope)
                    , m21 = 0
                    , m31 = 0
                    , m41 = 0
                    , m12 = 0
                    , m22 = 1 / frustumSlope
                    , m32 = 0
                    , m42 = 0
                    , m13 = 0
                    , m23 = 0
                    , m33 = -(f + n) / (f - n)
                    , m43 = -1
                    , m14 = 0
                    , m24 = 0
                    , m34 = -2 * f * n / (f - n)
                    , m44 = 0
                    }

        Types.Orthographic (Quantity viewportHeight) ->
            if isInfinite f then
                Math.Matrix4.fromRecord
                    { m11 = 2 / (aspectRatio * viewportHeight)
                    , m21 = 0
                    , m31 = 0
                    , m41 = 0
                    , m12 = 0
                    , m22 = 2 / viewportHeight
                    , m32 = 0
                    , m42 = 0
                    , m13 = 0
                    , m23 = 0
                    , m33 = 0
                    , m43 = 0
                    , m14 = 0
                    , m24 = 0
                    , m34 = -1
                    , m44 = 1
                    }

            else
                Math.Matrix4.fromRecord
                    { m11 = 2 / (aspectRatio * viewportHeight)
                    , m21 = 0
                    , m31 = 0
                    , m41 = 0
                    , m12 = 0
                    , m22 = 2 / viewportHeight
                    , m32 = 0
                    , m42 = 0
                    , m13 = 0
                    , m23 = 0
                    , m33 = -2 / (f - n)
                    , m43 = 0
                    , m14 = 0
                    , m24 = 0
                    , m34 = -(f + n) / (f - n)
                    , m44 = 1
                    }


{-| Construct a WebGL view-projection matrix for a given camera; this is the
product of the projection and view matrices. Multiplying by this matrix
converts from world coordinates to clip coordinates.
-}
viewProjectionMatrix :
    Camera3d units coordinates
    ->
        { nearClipDepth : Quantity Float units
        , farClipDepth : Quantity Float units
        , aspectRatio : Float
        }
    -> Mat4
viewProjectionMatrix camera projectionParameters =
    Math.Matrix4.mul
        (projectionMatrix camera projectionParameters)
        (viewMatrix (Camera3d.viewpoint camera))


{-| Construct a WebGL model-view-projection matrix for a given camera; this is
the product of the projection and model-view matrices. Multiplying by this
matrix converts from local object coordinates to clip coordinates.
-}
modelViewProjectionMatrix :
    Frame3d units coordinates defines
    -> Camera3d units coordinates
    ->
        { nearClipDepth : Quantity Float units
        , farClipDepth : Quantity Float units
        , aspectRatio : Float
        }
    -> Mat4
modelViewProjectionMatrix modelFrame camera projectionParameters =
    Math.Matrix4.mul
        (projectionMatrix camera projectionParameters)
        (modelViewMatrix modelFrame (Camera3d.viewpoint camera))
