module Frontend exposing (app)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Game
import Guid exposing (Guid)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lamdera
import Pixels
import Route exposing (Route)
import Scene3d.Material as Material
import Task
import Types exposing (..)
import Url exposing (Url)
import WebGL.Texture exposing (defaultOptions)


app =
    Lamdera.frontend
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = ClickedLink
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Lamdera"
                , body = [ view model ]
                }
        }



-- Model


type alias Model =
    FrontendModel


unwrapGameModel : Model -> Maybe Game.Model
unwrapGameModel model =
    case model.status of
        Loading _ _ ->
            Nothing

        StartMenu _ gameModel ->
            Just gameModel

        Running gameModel ->
            Just gameModel

        NetworkPlay _ gameModel _ _ ->
            Just gameModel

        Failed _ ->
            Nothing



-- Init


init : Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { status =
            Loading
                { dimensions = Nothing
                , roughnessTexture = Nothing
                , ballTextures = Dict.empty
                }
                url
      , key = key
      , url = url
      }
    , Cmd.batch
        [ Cmd.batch
            (List.map
                (\number ->
                    Material.loadWith
                        { defaultOptions | minify = WebGL.Texture.linear }
                        ("/img/balls/" ++ String.fromInt number ++ ".png")
                        |> Task.attempt (GotBallTexture number)
                )
                (List.range 1 15)
            )
        , Task.attempt GotRoughnessTexture (Material.load "/img/roughness.jpg")
        , Task.perform GotInitialViewport Browser.Dom.getViewport
        ]
    )



-- View


view : Model -> Html.Html FrontendMsg
view model =
    case model.status of
        StartMenu generateLinkStatus gameModel ->
            viewStartMenu model.url
                gameModel
                generateLinkStatus

        Running gameModel ->
            Html.map RunningMsg (Game.view gameModel)

        NetworkPlay _ gameModel player _ ->
            viewNetworkPlay
                gameModel
                player

        Loading _ _ ->
            Html.text "Loading..."

        Failed error ->
            Html.text error


fromGamePlayer : Int -> Maybe Player
fromGamePlayer gamePlayer =
    case gamePlayer of
        0 ->
            Just Player1

        1 ->
            Just Player2

        _ ->
            Nothing


viewStartMenu : Url -> Game.Model -> GenerateLinkStatus -> Html.Html Msg
viewStartMenu url gameModel generateLinkStatus =
    Html.div
        []
        [ Html.map RunningMsg (Game.view gameModel)
        , fontStyle
        , case generateLinkStatus of
            LinkNotRequested ->
                viewMenuOptions

            LinkGenerating ->
                viewGeneratingLink

            LinkGenerated link ->
                viewLinkGenerated url link
        ]


fontStyle : Html msg
fontStyle =
    Html.node "style"
        []
        [ Html.text <| """
@font-face {
    font-family: 'Teko';
    src: url('/assets/Teko-Medium.woff2') format('woff2'),
        url('/assets/Teko-Medium.woff') format('woff');
    font-weight: 500;
    font-style: normal;
    font-display: block;
}
"""
        ]


viewMenuOptions : Html.Html Msg
viewMenuOptions =
    Html.div
        (menuStyle
            ++ [ Html.Attributes.style "flex-direction" "column"
               ]
        )
        [ Html.h1 []
            [ Html.text "How would you like to play?"
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            ]
            [ viewOption
                { label = "Local play"
                , onSelect = LocalPlaySelected
                }
            , Html.div
                [ Html.Attributes.style "padding" "15px 25px 10px 25px"
                ]
                [ Html.text "OR"
                ]
            , viewOption
                { label = "Network play"
                , onSelect = NetworkPlaySelected
                }
            ]
        ]


viewOption : { label : String, onSelect : Msg } -> Html Msg
viewOption { label, onSelect } =
    Html.div
        [ Html.Attributes.style "border-radius" "10px"
        , Html.Attributes.style "box-shadow" "0 4px 12px 0 rgba(9, 9, 9, 0.15)"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "background-color" "#020507"

        -- , Html.Attributes.style "filter" "brightness(50%)"
        , Html.Attributes.style "padding" "15px 25px 10px 25px"
        , Html.Events.onClick onSelect
        ]
        [ Html.text label
        ]


menuStyle : List (Html.Attribute Msg)
menuStyle =
    [ -- To center within screen.
      Html.Attributes.style "height" "100vh"
    , Html.Attributes.style "width" "100vw"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "justify-content" "center"
    , -- Color
      Html.Attributes.style "background-color" "#121517"
    , Html.Attributes.style "color" "#eef"

    -- Border/Edges
    , Html.Attributes.style "border-radius" "10px 10px 0 0"
    , Html.Attributes.style "box-shadow" "0 4px 12px 0 rgba(0, 0, 0, 0.15)"

    -- Font
    , Html.Attributes.style "font-family" "Teko"
    , Html.Attributes.style "font-size" "40px"

    -- Other
    , Html.Attributes.style "opacity" "0.6"
    ]


viewGeneratingLink : Html.Html Msg
viewGeneratingLink =
    Html.div
        (menuStyle
            ++ [ Html.Attributes.style "flex-direction" "column"
               ]
        )
        [ Html.h1 []
            [ Html.text "Generating link..."
            ]
        ]


viewLinkGenerated : Url -> Guid -> Html.Html Msg
viewLinkGenerated url link =
    let
        urlPrefix =
            { url
                | path = ""
                , fragment = Nothing
                , query = Nothing
            }
    in
    Html.div
        (menuStyle
            ++ [ Html.Attributes.style "flex-direction" "column"
               ]
        )
        [ Html.label []
            [ Html.h1 [] [ Html.text "Awaiting opponent..." ]
            , Html.text "Share this link with a friend"
            , Html.pre
                [ Html.Attributes.style "user-select" "all"
                , Html.Attributes.style "font-size" "20px"
                ]
                [ Html.text <|
                    Url.toString urlPrefix
                        ++ "/play/"
                        ++ Guid.toString link
                ]
            ]
        ]



-- Network play


viewNetworkPlay : Game.Model -> Player -> Html Msg
viewNetworkPlay gameModel player =
    Html.div []
        [ viewGame gameModel player
        , viewCurrentStatus gameModel player
        ]


viewGame : Game.Model -> Player -> Html Msg
viewGame gameModel player =
    let
        maybeCurrentPlayer =
            fromGamePlayer (Game.currentPlayer gameModel)
    in
    case maybeCurrentPlayer of
        Nothing ->
            Html.map RunningMsg (Game.view gameModel)

        Just currentPlayer ->
            if player == currentPlayer then
                Html.map MyPlayerRunningMsg (Game.view gameModel)

            else
                Html.map RunningMsg (Game.view gameModel)



-- View Current Status


viewCurrentStatus : Game.Model -> Player -> Html Msg
viewCurrentStatus gameModel myPlayer =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "0"
        , Html.Attributes.style "left" "50%" -- To center absolute content.
        ]
        [ Html.node "style"
            []
            [ fontStyle
            ]
        , case gameModel.state of
            Game.GameOver _ _ ->
                viewGameOver gameModel myPlayer

            _ ->
                Html.div statusStyle
                    [ Html.text (currentPlayerString gameModel myPlayer)
                    , Html.text " - "
                    , Html.text (Game.currentTarget gameModel.state)
                    ]
        ]


currentPlayerString : Game.Model -> Player -> String
currentPlayerString gameModel myPlayer =
    let
        playerIndex =
            Game.currentPlayer gameModel
    in
    case ( playerIndex, myPlayer ) of
        ( 0, Player1 ) ->
            "You"

        ( 1, Player2 ) ->
            "You"

        _ ->
            "Other player"


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


viewGameOver : Game.Model -> Player -> Html Msg
viewGameOver gameModel myPlayer =
    Html.button
        (statusStyle
            ++ [ Html.Attributes.style "border" "none"
               , Html.Attributes.style "cursor" "pointer"

               -- , Html.Events.onClick StartNewGameButtonClicked
               ]
        )
        [ Html.text (currentPlayerString gameModel myPlayer)
        , Html.text " won!"

        -- , Html.div
        --     [ Html.Attributes.style "color" "rgba(86, 186, 79, 0.7)"
        --     ]
        --     [ Html.text "Play again?"
        --     ]
        ]



-- Update


type alias Msg =
    FrontendMsg


update : FrontendMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialViewport { viewport } ->
            case model.status of
                Loading loadingModel url ->
                    loadComplete url
                        { loadingModel
                            | dimensions = Just ( viewport.width, viewport.height )
                        }
                        model

                _ ->
                    ( model, Cmd.none )

        GotBallTexture n maybeTexture ->
            case model.status of
                Loading loadingModel url ->
                    case maybeTexture of
                        Ok texture ->
                            loadComplete url
                                { loadingModel
                                    | ballTextures = Dict.insert n texture loadingModel.ballTextures
                                }
                                model

                        Err _ ->
                            ( { model
                                | status = Failed "Failed to load ball texture"
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        GotRoughnessTexture maybeTexture ->
            case model.status of
                Loading loadingModel url ->
                    case maybeTexture of
                        Ok texture ->
                            loadComplete url
                                { loadingModel
                                    | roughnessTexture = Just texture
                                }
                                model

                        Err _ ->
                            ( { model
                                | status = Failed "Failed to load roughness texture"
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        RunningMsg runningMsg ->
            case model.status of
                Running runningModel ->
                    let
                        ( newGameModel, _ ) =
                            Game.update runningMsg runningModel
                    in
                    ( { model
                        | status = Running newGameModel
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MyPlayerRunningMsg runningMsg ->
            case model.status of
                NetworkPlay guid gameModel player playerActions ->
                    let
                        ( newGameModel, gameEffect ) =
                            Game.update runningMsg gameModel
                    in
                    ( { model
                        | status =
                            NetworkPlay
                                guid
                                newGameModel
                                player
                                playerActions
                      }
                    , performEffect guid gameEffect
                    )

                _ ->
                    ( model, Cmd.none )

        LocalPlaySelected ->
            case model.status of
                StartMenu _ gameModel ->
                    let
                        ( x, y ) =
                            gameModel.dimensions
                    in
                    ( { model
                        | status =
                            Running <|
                                Game.initial
                                    gameModel.ballTextures
                                    gameModel.roughnessTexture
                                    ( Pixels.toFloat x
                                    , Pixels.toFloat y
                                    )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NetworkPlaySelected ->
            case model.status of
                StartMenu LinkNotRequested gameModel ->
                    ( { model
                        | status = StartMenu LinkGenerating gameModel
                      }
                    , Lamdera.sendToBackend GenerateLink
                    )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            case model.status of
                Loading _ _ ->
                    ( model, Cmd.none )

                StartMenu _ _ ->
                    ( model, Cmd.none )

                Running _ ->
                    ( model, Cmd.none )

                NetworkPlay guid gameModel player playerActions ->
                    case ( gameModel.state, playerActions ) of
                        ( Game.Simulating _, _ ) ->
                            ( model, Cmd.none )

                        ( _, [] ) ->
                            ( model, Cmd.none )

                        ( _, firstPlayerAction :: otherPlayerActions ) ->
                            let
                                ( newGameModel, _ ) =
                                    updatePlayerAction firstPlayerAction gameModel
                            in
                            ( { model
                                | status = NetworkPlay guid newGameModel player otherPlayerActions
                              }
                            , Cmd.none
                            )

                Failed _ ->
                    ( model, Cmd.none )

        -- Routing
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            --- For us, this means we're directed to a non-SPA URI, since
                            --- for now we do hashed/fragmented routing.
                            ( model, Nav.load url.path )

                        Just _ ->
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                newModel =
                    { model
                        | url = url
                    }
            in
            case unwrapGameModel newModel of
                Nothing ->
                    ( newModel, Cmd.none )

                Just gameModel ->
                    routeTo url
                        newModel
                        gameModel


performEffect : Guid -> Game.Effect -> Cmd Msg
performEffect guid gameEffect =
    case gameEffect of
        Game.NoEffect ->
            Cmd.none

        Game.SetBallInHandPlace ballInHandPosition ->
            Lamdera.sendToBackend
                (PlayerAction guid (PlacedBallInHand ballInHandPosition))

        Game.SetBehindHeadStringPlace behindHeadstringPosition ->
            Lamdera.sendToBackend
                (PlayerAction guid (PlacedBallInKitchen behindHeadstringPosition))

        Game.Shoot axis duration camera ->
            Lamdera.sendToBackend
                (PlayerAction guid (PlayerShot axis duration camera))

        Game.CueMove cueElevation newCamera ->
            Lamdera.sendToBackend
                (PlayerAction guid (MovedCue cueElevation newCamera))


routeTo : Url -> Model -> Game.Model -> ( Model, Cmd Msg )
routeTo url model gameModel =
    case Route.fromUrl url of
        Nothing ->
            ( model, Cmd.none )

        Just route ->
            routeChange
                route
                gameModel
                model


routeChange : Route -> Game.Model -> Model -> ( Model, Cmd Msg )
routeChange route gameModel model =
    case route of
        Route.Home ->
            ( { model
                | status = StartMenu LinkNotRequested gameModel
              }
            , Cmd.none
            )

        Route.Play guid ->
            ( { model
                | status = NetworkPlay guid gameModel Player2 []
              }
            , Lamdera.sendToBackend (PlayerJoined guid)
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        GeneratedLink link ->
            case model.status of
                StartMenu LinkGenerating gameModel ->
                    ( { model
                        | status = StartMenu (LinkGenerated link) gameModel
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        OtherPlayerJoined ->
            case model.status of
                StartMenu (LinkGenerated guid) gameModel ->
                    ( { model
                        | status = NetworkPlay guid gameModel Player1 []
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        OtherPlayerAction action ->
            case model.status of
                NetworkPlay guid gameModel myPlayer playerActions ->
                    let
                        newPlayerActions =
                            playerActions ++ [ action ]
                    in
                    ( { model
                        | status =
                            NetworkPlay guid
                                gameModel
                                myPlayer
                                newPlayerActions
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


updatePlayerAction : PlayerAction -> Game.Model -> ( Game.Model, Game.Effect )
updatePlayerAction playerAction gameModel =
    case playerAction of
        PlacedBallInHand newCueBallLocation ->
            Game.updatePlacedBallInHand newCueBallLocation
                gameModel

        PlacedBallInKitchen newCueBallLocation ->
            Game.updatePlacedBallBehindHeadstring newCueBallLocation
                gameModel

        PlayerShot axis duration camera ->
            Game.updateShoot axis
                duration
                camera
                gameModel

        MovedCue cueData camera ->
            Game.updateCueMove cueData
                camera
                gameModel



-- Subscriptions


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.status of
        Running gameModel ->
            Sub.map RunningMsg (Game.subscriptions gameModel)

        NetworkPlay guid gameModel player playerActions ->
            let
                maybeCurrentPlayer =
                    fromGamePlayer (Game.currentPlayer gameModel)
            in
            case maybeCurrentPlayer of
                Nothing ->
                    Sub.none

                Just currentPlayer ->
                    if player == currentPlayer then
                        Sub.map MyPlayerRunningMsg (Game.subscriptions gameModel)

                    else
                        Sub.batch
                            [ Browser.Events.onAnimationFrame Tick
                            , Sub.map MyPlayerRunningMsg (Game.subscriptionsSimulating gameModel)
                            ]

        _ ->
            Sub.none



-- Http (for requests)


loadComplete : Url -> LoadingModel -> Model -> ( Model, Cmd Msg )
loadComplete url loadingModel model =
    if Dict.size loadingModel.ballTextures == 15 then
        Maybe.map2 (Game.initial loadingModel.ballTextures)
            loadingModel.roughnessTexture
            loadingModel.dimensions
            |> Maybe.map (routeTo url model)
            |> Maybe.withDefault
                ( { model
                    | status = Loading loadingModel url
                  }
                , Cmd.none
                )

    else
        ( { model
            | status = Loading loadingModel url
          }
        , Cmd.none
        )



-- Serialization (for encoding/decoding)
