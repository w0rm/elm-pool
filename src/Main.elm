module Main exposing (main)

import Browser
import Browser.Dom
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
    | Running String Game.Model
    | Failed String


type Msg
    = GotInitialViewport Browser.Dom.Viewport
    | GotBallTexture Int (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | RunningMsg Game.Msg
    | StartNewGameButtonClicked


type alias LoadingModel =
    { ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , window : Maybe (Rectangle2d Pixels Game.ScreenCoordinates)
    , assetsPath : String
    }


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    Running _ m ->
                        Sub.map RunningMsg (Game.subscriptions m)

                    _ ->
                        Sub.none
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
        , Task.perform GotInitialViewport Browser.Dom.getViewport
        ]
    )


view : Model -> Html Msg
view model =
    case model of
        Running assetsPath gameModel ->
            Html.div
                []
                [ Html.map RunningMsg (Game.view gameModel)
                , viewCurrentStatus gameModel
                    assetsPath
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
        ( GotInitialViewport { viewport }, Loading loadingModel ) ->
            loadComplete
                { loadingModel
                    | window =
                        Just
                            (Rectangle2d.with
                                { x1 = Pixels.pixels 0
                                , y1 = Pixels.pixels viewport.height
                                , x2 = Pixels.pixels viewport.width
                                , y2 = Pixels.pixels 0
                                }
                            )
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

        ( RunningMsg runningMsg, Running assetsPath runningModel ) ->
            let
                newGameModel =
                    Game.update runningMsg runningModel
            in
            Running assetsPath newGameModel

        ( StartNewGameButtonClicked, Running assetsPath gameModel ) ->
            Running assetsPath <|
                Game.initial
                    gameModel.ballTextures
                    gameModel.roughnessTexture
                    gameModel.window

        _ ->
            model


loadComplete : LoadingModel -> Model
loadComplete model =
    if Dict.size model.ballTextures == 15 then
        Maybe.map2 (Game.initial model.ballTextures)
            model.roughnessTexture
            model.window
            |> Maybe.map (Running model.assetsPath)
            |> Maybe.withDefault (Loading model)

    else
        Loading model
