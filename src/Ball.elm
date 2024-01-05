module Ball exposing (body, entity, rack, radius, spot)

import Angle
import Axis2d exposing (Axis2d)
import Axis3d
import Bodies exposing (Id(..))
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Direction2d
import Direction3d
import EightBall exposing (Ball)
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material as Material exposing (Material)
import Physics.World as World exposing (World)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d
import Vector2d
import Vector3d


radius : Length
radius =
    Length.millimeters (57.15 / 2)


weight : Mass
weight =
    Mass.grams 170


{-| The perfect sphere keeps rolling on the perfect plane,
we need to damp the angular velocity to make it stop.
-}
damping : { linear : Float, angular : Float }
damping =
    { linear = 0.4, angular = 0.4 }


material : Material
material =
    Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


body : id -> Body id
body id =
    Body.sphere (Sphere3d.atOrigin radius) id
        |> Body.withMaterial material
        |> Body.withDamping damping
        |> Body.withBehavior (Body.dynamic weight)
        -- rotate to see the numbers on the balls
        |> Body.rotateAround Axis3d.x (Angle.degrees 90)


entity : Material.Texture Color -> Material.Texture Float -> Entity BodyCoordinates
entity baseColor roughnessTexture =
    Scene3d.sphereWithShadow
        (Material.texturedPbr
            { baseColor = baseColor
            , roughness = roughnessTexture
            , metallic = Material.constant 0
            }
        )
        (Sphere3d.atOrigin radius)


{-| Rack the balls at the foot spot on the table
-}
rack : Point2d Meters WorldCoordinates -> List (Body Id)
rack footSpot =
    let
        -- TODO: randomly shuffle the balls?
        numbers =
            [ 1, 10, 4, 2, 8, 5, 9, 3, 14, 15, 11, 12, 6, 13, 7 ]
    in
    numbers
        |> List.indexedMap
            (\index number ->
                let
                    -- index:
                    --         14
                    --       9
                    --     5   13
                    --   2   8
                    -- 0   4   12
                    --   1   7
                    --     3   11
                    --       6
                    --         10
                    -- row:
                    -- 0 1 2 3 4
                    row =
                        round (sqrt (2 * (toFloat index + 1))) - 1

                    offset =
                        Vector3d.xyz
                            (Quantity.multiplyBy (toFloat row * sqrt 3) radius)
                            (Quantity.multiplyBy (toFloat (index * 2 - row * (row + 2))) radius)
                            radius

                    position =
                        footSpot
                            |> Point3d.on SketchPlane3d.xy
                            |> Point3d.translateBy offset
                in
                ( EightBall.numberedBall number |> Maybe.map Numbered, position )
            )
        |> List.filterMap
            (\( maybeId, pos ) ->
                Maybe.map (body >> Body.moveTo pos) maybeId
            )


{-| Place a spotted ball on the line behind the foot spot,
such that it doesn't collide with existing balls
-}
spot : Point2d Meters WorldCoordinates -> Ball -> World Id -> World Id
spot footSpot spottedBall world =
    let
        -- the line behind the foot spot
        axis =
            Axis2d.through footSpot Direction2d.x

        -- the distance from the center of the table to the foot spot
        -- is equal to the distance from the foot spot to the foot rail
        distanceToFootRail =
            Point2d.xCoordinate footSpot

        occupiedRange ballPosition =
            ballPosition
                |> Point3d.projectInto SketchPlane3d.xy
                |> Circle2d.withRadius (Quantity.twice radius)
                |> intersectBy axis

        -- list of occupied ranges on the line behind the foot spot,
        -- the endpoints are sorted along the axis, e.g. for balls a, b and c:
        --  a1 (f) a2  b1  b2  c1 c2      |
        --    foot spot   ----->      foot rail
        occupiedRanges =
            world
                |> World.bodies
                |> List.filterMap
                    (\b ->
                        case Body.data b of
                            Numbered _ ->
                                occupiedRange (Body.originPoint b)

                            CueBall ->
                                occupiedRange (Body.originPoint b)

                            _ ->
                                Nothing
                    )

        behindFootSpot =
            occupiedRanges
                -- collect the furthest endpoints
                |> List.map Tuple.second
                |> List.filter
                    (\point ->
                        Quantity.greaterThan Quantity.zero point
                            && Quantity.lessThan distanceToFootRail point
                    )
                -- sort based on the distance to the foot spot
                |> List.sortBy Quantity.unwrap

        inFrontOfFootSpot =
            occupiedRanges
                -- collect the nearest endpoints
                |> List.map Tuple.first
                |> List.filter (Quantity.lessThan Quantity.zero)
                -- sort based on the distance to the foot spot
                |> List.sortBy (Quantity.unwrap >> negate)

        spawnLocation =
            (Quantity.zero :: behindFootSpot ++ inFrontOfFootSpot)
                |> List.filter
                    (\distance ->
                        List.all
                            (\( start, end ) ->
                                Quantity.lessThanOrEqualTo start distance
                                    || Quantity.greaterThanOrEqualTo end distance
                            )
                            occupiedRanges
                    )
                |> List.head
                -- should never happen, would result in overlapping balls!
                |> Maybe.withDefault Quantity.zero
                |> Point2d.along axis
                |> Point3d.on SketchPlane3d.xy
                |> Point3d.translateIn Direction3d.z radius
    in
    world
        |> World.add (body (Numbered spottedBall) |> Body.moveTo spawnLocation)


intersectBy : Axis2d Meters coordinates -> Circle2d Meters coordinates -> Maybe ( Length, Length )
intersectBy axis circle =
    let
        axisOrigin =
            Axis2d.originPoint axis

        axisDirection =
            Axis2d.direction axis

        centerPoint =
            Circle2d.centerPoint circle

        circleCenterToOrigin =
            Vector2d.from centerPoint axisOrigin

        cto =
            Vector2d.toMeters circleCenterToOrigin

        ctoLengthSquared =
            cto.x ^ 2 + cto.y ^ 2

        dotProduct =
            Vector2d.componentIn axisDirection circleCenterToOrigin |> Length.inMeters

        r =
            Circle2d.radius circle |> Length.inMeters

        inRoot =
            dotProduct ^ 2 - ctoLengthSquared + r ^ 2
    in
    if inRoot < 0 then
        Nothing

    else
        let
            d1 =
                (-dotProduct - sqrt inRoot) |> Length.meters

            d2 =
                (-dotProduct + sqrt inRoot) |> Length.meters
        in
        Just ( d1, d2 )
