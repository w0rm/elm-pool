module Types exposing (..)

import Axis3d exposing (Axis3d)
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Color exposing (Color)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Game exposing (Camera)
import Guid exposing (Guid)
import Lamdera exposing (ClientId)
import Length exposing (Meters)
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d exposing (Point3d)
import Scene3d.Material as Material
import Time
import Url exposing (Url)
import WebGL.Texture


type alias FrontendModel =
    { key : Nav.Key
    , url : Url
    , status : FrontendStatus
    }


type FrontendStatus
    = Loading LoadingModel Url
    | StartMenu GenerateLinkStatus Game.Model
      -- | AwaitingAnotherPlayer Game.Model Link
    | Running Game.Model
    | NetworkPlay Guid Game.Model Player (List PlayerAction)
    | Failed String


type Player
    = Player1
    | Player2


type alias LoadingModel =
    { ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , dimensions : Maybe ( Float, Float )
    }


type GenerateLinkStatus
    = LinkNotRequested
    | LinkGenerating
    | LinkGenerated Guid


type alias BackendModel =
    { message : String
    , links : Dict String PlayLink
    }


type alias PlayLink =
    { owner : ClientId
    , sharedTo : Maybe ClientId
    }


type FrontendMsg
    = GotInitialViewport Browser.Dom.Viewport
    | GotBallTexture Int (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | RunningMsg Game.Msg
    | MyPlayerRunningMsg Game.Msg
    | NoOpFrontendMsg
    | LocalPlaySelected
    | NetworkPlaySelected
    | Tick Time.Posix
      -- Routing
    | UrlChanged Url
    | ClickedLink Browser.UrlRequest


type ToBackend
    = GenerateLink
    | PlayerJoined Guid
    | PlayerAction Guid PlayerAction


type PlayerAction
    = PlacedBallInHand (Point3d Meters WorldCoordinates)
    | PlacedBallInKitchen (Point3d Meters WorldCoordinates)
    | PlayerShot (Axis3d Meters WorldCoordinates) Duration Camera
    | MovedCue (Maybe Game.CueData) Camera


type BackendMsg
    = NoOpBackendMsg
    | RandomGeneratedLinkBackend ClientId Guid


type ToFrontend
    = GeneratedLink Guid
    | OtherPlayerJoined
    | OtherPlayerAction PlayerAction
