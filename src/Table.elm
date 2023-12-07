module Table exposing
    ( Table
    , areaBallInHand
    , areaBehindTheHeadString
    , areaBehindTheHeadStringEntity
    , footSpot
    , load
    )

{-| elm-obj-file is used to decode various objects from the obj file.

  - Colliders for the table body prefixed with `Table-*`
  - Colliders for the cushions body prefixed with `Cushions-*`
  - Collider for the pockets body: `Pockets`
  - Mesh used for rendering: `Billiard-Table`

The Billiard Table model is designed by Kolja Wilcke <https://twitter.com/01k>

-}

import Ball
import Bodies exposing (Id(..))
import Color
import Direction3d
import Frame3d
import Http
import Length exposing (Length, Meters)
import Obj.Decode exposing (Decoder)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.Shape as Shape
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Scene3d.Mesh
import SketchPlane3d
import Task exposing (Task)


type alias Table =
    { bodies : List (Body Id)
    , entity : Entity BodyCoordinates
    }


load : { colorTexture : String, roughnessTexture : String, metallicTexture : String, mesh : String } -> Task String Table
load urls =
    Http.task
        { method = "get"
        , headers = []
        , body = Http.emptyBody
        , url = urls.mesh
        , resolver =
            Http.stringResolver
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ str ->
                            Obj.Decode.decodeString Length.meters tableDecoder str

                        _ ->
                            Err "Failed to load mesh"
                )
        , timeout = Nothing
        }
        |> Task.andThen
            (\fn ->
                Task.map3
                    (\colorTexture _ metallicTexture ->
                        fn
                            (Material.texturedPbr
                                { baseColor = colorTexture
                                , roughness = Material.constant 0.8 -- roughnessTexture
                                , metallic = metallicTexture
                                }
                            )
                    )
                    (Material.loadWith Material.nearestNeighborFiltering urls.colorTexture)
                    (Material.load urls.roughnessTexture)
                    (Material.load urls.metallicTexture)
                    |> Task.mapError (\_ -> "Failed to load texture")
            )


tableDecoder : Decoder (Material.Textured BodyCoordinates -> Table)
tableDecoder =
    Obj.Decode.map4
        (\tableConvexes cushionsConvexes pocketsConvex visual ->
            let
                mesh =
                    Scene3d.Mesh.texturedFaces visual

                bodies =
                    [ Body.plane Floor
                        -- distance from the floor until the top of the table
                        |> Body.moveTo (Point3d.meters 0 0 -0.45)
                    , Body.compound (List.map Shape.unsafeConvex tableConvexes) Bodies.Table
                        |> Body.withMaterial
                            (Physics.Material.custom
                                { friction = 0.8
                                , bounciness = 0
                                }
                            )
                    , Body.compound (List.map Shape.unsafeConvex cushionsConvexes) Cushion
                        |> Body.withMaterial
                            (Physics.Material.custom
                                { friction = 0.1
                                , bounciness = 0.8
                                }
                            )
                    , Body.compound [ Shape.unsafeConvex pocketsConvex ] Pocket
                    ]
            in
            \material ->
                { bodies = bodies
                , entity =
                    Scene3d.group
                        [ Scene3d.meshWithShadow material mesh (Scene3d.Mesh.shadow mesh)
                        , -- floor
                          Scene3d.quad
                            (Material.matte (Color.rgb255 46 52 54))
                            (Point3d.meters -15 -15 0)
                            (Point3d.meters 15 -15 0)
                            (Point3d.meters 15 15 0)
                            (Point3d.meters -15 15 0)
                        ]
                }
        )
        (startsWith "Table-" (Obj.Decode.trianglesIn Frame3d.atOrigin))
        (startsWith "Cushions-" (Obj.Decode.trianglesIn Frame3d.atOrigin))
        (Obj.Decode.object "Pockets" (Obj.Decode.trianglesIn Frame3d.atOrigin))
        (Obj.Decode.object "Billiard-Table" (Obj.Decode.texturedFacesIn Frame3d.atOrigin))


startsWith : String -> Decoder a -> Decoder (List a)
startsWith prefix decoder =
    Obj.Decode.objectNames
        |> Obj.Decode.andThen
            (\names ->
                names
                    |> List.filter (String.startsWith prefix)
                    |> List.map (\name -> Obj.Decode.object name decoder)
                    |> Obj.Decode.combine
            )


areaBallInHand : Rectangle3d Meters WorldCoordinates
areaBallInHand =
    let
        xOffset =
            Quantity.half length |> Quantity.minus Ball.radius

        yOffset =
            Quantity.half width |> Quantity.minus Ball.radius
    in
    Rectangle3d.on SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.xy (Quantity.negate xOffset) (Quantity.negate yOffset))
            (Point2d.xy xOffset yOffset)
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


length : Length
length =
    Length.meters 2.26


width : Length
width =
    Length.meters 1.24


footSpot : Point2d Meters WorldCoordinates
footSpot =
    Point2d.xy
        (Quantity.half (Quantity.half length))
        Quantity.zero


areaBehindTheHeadString : Rectangle3d Meters WorldCoordinates
areaBehindTheHeadString =
    let
        yOffset =
            Quantity.half width |> Quantity.minus Ball.radius

        xMin =
            Quantity.half length |> Quantity.minus Ball.radius |> Quantity.negate

        xMax =
            Quantity.half (Quantity.half length) |> Quantity.negate
    in
    Rectangle3d.on SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.xy xMin (Quantity.negate yOffset))
            (Point2d.xy xMax yOffset)
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


{-| Highlight the area where the ball should be placed
-}
areaBehindTheHeadStringEntity : Entity WorldCoordinates
areaBehindTheHeadStringEntity =
    case Rectangle3d.vertices areaBehindTheHeadString of
        [ v1, v2, v3, v4 ] ->
            Scene3d.quad
                (Material.nonmetal
                    { baseColor = Color.rgb255 131 146 34
                    , roughness = 1
                    }
                )
                v1
                v2
                v3
                v4

        _ ->
            Scene3d.nothing
