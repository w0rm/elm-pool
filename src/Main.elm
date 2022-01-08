module Main exposing (main)

import Browser
import Browser.Dom
import Color exposing (Color)
import Dict exposing (Dict)
import Game
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pixels
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
    | StartNewGameButtonClicked


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
        , view = view
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


view : Model -> Html Msg
view model =
    case model of
        Running gameModel ->
            Html.div
                []
                [ Html.map RunningMsg (Game.view gameModel)
                , viewCurrentStatus gameModel
                ]

        Loading _ ->
            Html.text "Loading..."

        Failed error ->
            Html.text error



-- View Current Status


viewCurrentStatus : Game.Model -> Html Msg
viewCurrentStatus gameModel =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "0"
        , Html.Attributes.style "left" "50%" -- To center absolute content.
        ]
        [ Html.node "style"
            []
            [ Html.text """
@font-face {
    font-family: 'Teko';
    src: url('assets/Teko-Medium.woff2') format('woff2'),
        url('assets/Teko-Medium.woff') format('woff');
    font-weight: 500;
    font-style: normal;
    font-display: block;
}
"""
            ]
        , case gameModel.state of
            Game.GameOver _ _ ->
                viewGameOver gameModel

            _ ->
                Html.div statusStyle
                    [ Html.text (currentPlayerString gameModel)
                    , Html.text " - "
                    , Html.text (Game.currentTarget gameModel.state)
                    ]
        ]


currentPlayerString : Game.Model -> String
currentPlayerString gameModel =
    let
        playerIndex =
            Game.currentPlayer gameModel
    in
    case playerIndex of
        0 ->
            "Player 1"

        1 ->
            "Player 2"

        _ ->
            "Unknown"


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
        [ Html.text (currentPlayerString gameModel)
        , Html.text " won!"
        , Html.div
            [ Html.Attributes.style "color" "rgba(86, 186, 79, 0.7)"
            ]
            [ Html.text "Play again?"
            ]
        ]


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
            let
                ( newGameModel, _ ) =
                    Game.update runningMsg runningModel
            in
            Running newGameModel

        ( StartNewGameButtonClicked, Running gameModel ) ->
            let
                ( x, y ) =
                    gameModel.dimensions
            in
            Running <|
                Game.initial
                    gameModel.ballTextures
                    gameModel.roughnessTexture
                    ( Pixels.toFloat x
                    , Pixels.toFloat y
                    )

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
