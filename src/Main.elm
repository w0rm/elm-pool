module Main exposing (main)

import Browser
import Browser.Dom
import Color exposing (Color)
import Dict exposing (Dict)
import Game
import Html
import Scene3d.Material as Material
import Task
import WebGL.Texture exposing (defaultOptions)


type Model
    = Loading LoadingModel
    | Running Game.Model
    | Failed String


type Msg
    = GotInitialViewport Browser.Dom.Viewport
    | GotBallTexture Int (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | RunningMsg Game.Msg


type alias LoadingModel =
    { ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , dimensions : Maybe ( Float, Float )
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    Running m ->
                        Sub.map RunningMsg (Game.subscriptions m)

                    _ ->
                        Sub.none
        , view =
            \model ->
                case model of
                    Running m ->
                        Html.map RunningMsg (Game.view m)

                    Loading _ ->
                        Html.text "Loading..."

                    Failed error ->
                        Html.text error
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
        { dimensions = Nothing
        , roughnessTexture = Nothing
        , ballTextures = Dict.empty
        }
    , Cmd.batch
        [ Cmd.batch
            (List.map
                (\number ->
                    Material.loadWith
                        { defaultOptions | minify = WebGL.Texture.linear }
                        ("img/balls/" ++ String.fromInt number ++ ".png")
                        |> Task.attempt (GotBallTexture number)
                )
                (List.range 1 15)
            )
        , Task.attempt GotRoughnessTexture (Material.load "img/roughness.jpg")
        , Task.perform GotInitialViewport Browser.Dom.getViewport
        ]
    )


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( GotInitialViewport { viewport }, Loading loadingModel ) ->
            loadComplete
                { loadingModel
                    | dimensions = Just ( viewport.width, viewport.height )
                }

        ( GotBallTexture n maybeTexture, Loading loadingModel ) ->
            case maybeTexture of
                Ok texture ->
                    loadComplete
                        { loadingModel
                            | ballTextures = Dict.insert n texture loadingModel.ballTextures
                        }

                Err _ ->
                    Failed "Failed to load ball texture"

        ( GotRoughnessTexture maybeTexture, Loading loadingModel ) ->
            case maybeTexture of
                Ok texture ->
                    loadComplete
                        { loadingModel
                            | roughnessTexture = Just texture
                        }

                Err _ ->
                    Failed "Failed to load roughness texture"

        ( RunningMsg runningMsg, Running runningModel ) ->
            Running (Game.update runningMsg runningModel)

        _ ->
            model


loadComplete : LoadingModel -> Model
loadComplete model =
    if Dict.size model.ballTextures == 15 then
        Maybe.map2 (Game.initial model.ballTextures)
            model.roughnessTexture
            model.dimensions
            |> Maybe.map Running
            |> Maybe.withDefault (Loading model)

    else
        Loading model
