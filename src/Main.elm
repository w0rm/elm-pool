module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Camera exposing (ScreenCoordinates)
import Color exposing (Color)
import Dict exposing (Dict)
import Game
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Pixels exposing (Pixels)
import Rectangle2d exposing (Rectangle2d)
import Scene3d.Material as Material
import Task
import WebGL.Texture exposing (defaultOptions)


type Model
    = Loading LoadingModel
    | Loaded LoadedModel
    | Failed String


type Msg
    = WindowResized (Rectangle2d Pixels ScreenCoordinates)
    | GotBallTexture Int (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | RunningMsg Game.Msg
    | StartNewGameButtonClicked


type alias LoadingModel =
    { ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , window : Maybe (Rectangle2d Pixels ScreenCoordinates)
    , assetsPath : String
    }


type alias LoadedModel =
    { ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Material.Texture Float
    , window : Rectangle2d Pixels ScreenCoordinates
    , assetsPath : String
    , game : Game.Model
    }


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \model ->
                Sub.batch
                    [ Browser.Events.onResize
                        (\width height ->
                            WindowResized
                                (Rectangle2d.with
                                    { x1 = Pixels.pixels 0
                                    , y1 = Pixels.float (toFloat height)
                                    , x2 = Pixels.float (toFloat width)
                                    , y2 = Pixels.pixels 0
                                    }
                                )
                        )
                    , case model of
                        Loaded m ->
                            Sub.map RunningMsg (Game.subscriptions m.game)

                        _ ->
                            Sub.none
                    ]
        , view = view
        }


type alias Flags =
    { assetsPath : Maybe String
    }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map Flags
        assetsPathDecoder


assetsPathDecoder : Json.Decode.Decoder (Maybe String)
assetsPathDecoder =
    Json.Decode.maybe <|
        Json.Decode.field "assetsPath" Json.Decode.string


init : Json.Encode.Value -> ( Model, Cmd Msg )
init unsafeFlags =
    let
        flags =
            unsafeFlags
                |> Json.Decode.decodeValue flagsDecoder
                |> Result.withDefault { assetsPath = Nothing }

        assetsPath =
            Maybe.withDefault "/public/" flags.assetsPath
    in
    ( Loading
        { window = Nothing
        , roughnessTexture = Nothing
        , ballTextures = Dict.empty
        , assetsPath = assetsPath
        }
    , Cmd.batch
        [ Cmd.batch
            (List.map
                (\number ->
                    Material.loadWith
                        { defaultOptions | minify = WebGL.Texture.linear }
                        (assetsPath ++ "img/balls/" ++ String.fromInt number ++ ".png")
                        |> Task.attempt (GotBallTexture number)
                )
                (List.range 1 15)
            )
        , Task.attempt GotRoughnessTexture (Material.load (assetsPath ++ "img/roughness.jpg"))
        , Task.perform
            (\{ viewport } ->
                WindowResized
                    (Rectangle2d.with
                        { x1 = Pixels.pixels 0
                        , y1 = Pixels.pixels viewport.height
                        , x2 = Pixels.pixels viewport.width
                        , y2 = Pixels.pixels 0
                        }
                    )
            )
            Browser.Dom.getViewport
        ]
    )


view : Model -> Html Msg
view model =
    case model of
        Loaded { game, roughnessTexture, ballTextures, window, assetsPath } ->
            Html.div
                []
                [ Html.map RunningMsg
                    (Game.view ballTextures roughnessTexture window game)
                , viewCurrentStatus game assetsPath
                ]

        Loading _ ->
            Html.text "Loading..."

        Failed error ->
            Html.text error



-- View Current Status


viewCurrentStatus : Game.Model -> String -> Html Msg
viewCurrentStatus gameModel assetsPath =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "0"
        , Html.Attributes.style "left" "50%" -- To center absolute content.
        ]
        [ Html.node "style"
            []
            [ Html.text (fontStyle assetsPath) ]
        , case gameModel.state of
            Game.GameOver _ _ ->
                viewGameOver gameModel

            _ ->
                Html.div statusStyle
                    [ Html.text (Game.currentPlayer gameModel.state)
                    , Html.text " - "
                    , Html.text (Game.currentTarget gameModel.state)
                    ]
        ]


statusStyle : List (Html.Attribute Msg)
statusStyle =
    [ -- To center within absolute container.
      Html.Attributes.style "position" "relative"
    , Html.Attributes.style "left" "-50%"
    , Html.Attributes.style "text-align" "center"

    -- Color
    , Html.Attributes.style "background-color" "#121517"
    , Html.Attributes.style "color" "#eef"

    -- Border/Edges
    , Html.Attributes.style "border-radius" "10px 10px 0 0"
    , Html.Attributes.style "box-shadow" "0 4px 12px 0 rgba(0, 0, 0, 0.15)"

    -- Font
    , Html.Attributes.style "font-family" "Teko"
    , Html.Attributes.style "font-size" "40px"

    -- Other
    , Html.Attributes.style "opacity" "0.9"
    , Html.Attributes.style "padding" "15px 25px 10px 25px"
    ]


viewGameOver : Game.Model -> Html Msg
viewGameOver gameModel =
    Html.button
        (statusStyle
            ++ [ Html.Attributes.style "border" "none"
               , Html.Attributes.style "cursor" "pointer"
               , Html.Events.onClick StartNewGameButtonClicked
               ]
        )
        [ Html.text (Game.currentPlayer gameModel.state)
        , Html.text " won!"
        , Html.div
            [ Html.Attributes.style "color" "rgba(86, 186, 79, 0.7)"
            ]
            [ Html.text "Play again?"
            ]
        ]


fontStyle : String -> String
fontStyle path =
    """
@font-face {
    font-family: 'Teko';
    src: url('""" ++ path ++ """assets/Teko-Medium.woff2') format('woff2'),
        url('""" ++ path ++ """assets/Teko-Medium.woff') format('woff');
    font-weight: 500;
    font-style: normal;
    font-display: block;
}
"""



-- Update


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( WindowResized window, Loading loadingModel ) ->
            loadComplete { loadingModel | window = Just window }

        ( WindowResized window, Loaded loadedModel ) ->
            Loaded { loadedModel | window = window }

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

        ( RunningMsg runningMsg, Loaded loadedModel ) ->
            let
                newGame =
                    Game.update loadedModel.window runningMsg loadedModel.game
            in
            Loaded { loadedModel | game = newGame }

        ( StartNewGameButtonClicked, Loaded loadedModel ) ->
            Loaded { loadedModel | game = Game.initial }

        _ ->
            model


loadComplete : LoadingModel -> Model
loadComplete model =
    if Dict.size model.ballTextures == 15 then
        Maybe.map2
            (\roughnessTexture window ->
                Loaded
                    { ballTextures = model.ballTextures
                    , assetsPath = model.assetsPath
                    , window = window
                    , roughnessTexture = roughnessTexture
                    , game = Game.initial
                    }
            )
            model.roughnessTexture
            model.window
            |> Maybe.withDefault (Loading model)

    else
        Loading model
