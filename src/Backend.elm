module Backend exposing (app)

import Dict
import Guid
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Random
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


type alias Model =
    BackendModel


init : ( Model, Cmd BackendMsg )
init =
    ( { message = ""
      , links = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        RandomGeneratedLinkBackend clientId generatedLink ->
            ( { model
                | links =
                    model.links
                        |> Dict.insert (Guid.toString generatedLink)
                            { owner = clientId
                            , sharedTo = Nothing
                            }
              }
            , Lamdera.sendToFrontend clientId (GeneratedLink generatedLink)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GenerateLink ->
            -- TODO: Generate link
            let
                generatedLink =
                    "asdf"
            in
            ( model
            , Random.generate (RandomGeneratedLinkBackend clientId) Guid.generator
            )

        PlayerJoined guid ->
            let
                linkId =
                    Guid.toString guid
            in
            case Dict.get linkId model.links of
                Nothing ->
                    ( model, Cmd.none )

                Just playLink ->
                    ( { model
                        | links =
                            model.links
                                |> Dict.update linkId
                                    (Maybe.map (playLinkSetSharedTo clientId))
                      }
                    , Lamdera.sendToFrontend playLink.owner OtherPlayerJoined
                    )

        PlayerAction guid action ->
            let
                linkId =
                    Guid.toString guid

                maybePlayLink =
                    Dict.get linkId model.links

                maybeSharedTo =
                    Maybe.andThen .sharedTo
                        maybePlayLink
            in
            case ( maybePlayLink, maybeSharedTo ) of
                ( Just playLink, Just sharedTo ) ->
                    let
                        otherClientId =
                            if playLink.owner == clientId then
                                sharedTo

                            else
                                playLink.owner
                    in
                    ( model
                    , Lamdera.sendToFrontend otherClientId (OtherPlayerAction action)
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )


playLinkSetSharedTo : ClientId -> PlayLink -> PlayLink
playLinkSetSharedTo clientId playLink =
    { playLink
        | sharedTo = Just clientId
    }


subscriptions model =
    Sub.batch
        [--Lamdera.onConnect ClientConnected
        ]
