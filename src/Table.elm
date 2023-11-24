module Table exposing (Table, load)

{-| elm-obj-file is used to decode various objects from the obj file.

  - Colliders for the table body prefixed with `Table-*`
  - Colliders for the cushions body prefixed with `Cushions-*`
  - Collider for the pockets body: `Pockets`
  - Mesh used for rendering: `Billiard-Table`

The Billiard Table model is designed by Kolja Wilcke <https://twitter.com/01k>

-}

import Frame3d
import Http
import Length
import Obj.Decode exposing (Decoder)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Shape as Shape exposing (Shape)
import Scene3d.Material as Material
import Scene3d.Mesh exposing (Shadow, Textured)
import Task exposing (Task)


type alias Table =
    { table : List Shape
    , cushions : List Shape
    , pockets : List Shape
    , mesh : Textured BodyCoordinates
    , shadow : Shadow BodyCoordinates
    , material : Material.Textured BodyCoordinates
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
            in
            \material ->
                { table = List.map Shape.unsafeConvex tableConvexes
                , cushions = List.map Shape.unsafeConvex cushionsConvexes
                , pockets = [ Shape.unsafeConvex pocketsConvex ]
                , mesh = mesh
                , shadow = Scene3d.Mesh.shadow mesh
                , material = material
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
