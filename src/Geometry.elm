module Geometry exposing (intersectionWithRectangle, intersectionWithTriangle)

import Axis3d exposing (Axis3d)
import Plane3d
import Point3d exposing (Point3d)
import Rectangle3d exposing (Rectangle3d)
import SketchPlane3d
import Triangle2d
import Triangle3d exposing (Triangle3d)


intersectionWithRectangle : Axis3d units coordinates -> Rectangle3d units coordinates -> Maybe (Point3d units coordinates)
intersectionWithRectangle axis rectangle =
    case Rectangle3d.vertices rectangle of
        [ v1, v2, v3, v4 ] ->
            let
                triangle1 =
                    Triangle3d.fromVertices ( v1, v2, v3 )

                triangle2 =
                    Triangle3d.fromVertices ( v3, v4, v1 )
            in
            case intersectionWithTriangle axis triangle1 of
                Nothing ->
                    intersectionWithTriangle axis triangle2

                intersection ->
                    intersection

        _ ->
            Nothing


intersectionWithTriangle : Axis3d units coordinates -> Triangle3d units coordinates -> Maybe (Point3d units coordinates)
intersectionWithTriangle axis triangle =
    case Triangle3d.normalDirection triangle of
        Just normalDirection ->
            let
                ( p1, _, _ ) =
                    Triangle3d.vertices triangle

                plane =
                    Plane3d.through p1 normalDirection
            in
            case Axis3d.intersectionWithPlane plane axis of
                Just intersectionPoint ->
                    let
                        sketchPlane =
                            SketchPlane3d.fromPlane plane

                        projectedPoint =
                            Point3d.projectInto sketchPlane intersectionPoint

                        projectedTriangle =
                            Triangle3d.projectInto sketchPlane triangle
                    in
                    if Triangle2d.contains projectedPoint projectedTriangle then
                        Just intersectionPoint

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
