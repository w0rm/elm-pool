module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Color
import Dict
import Duration
import Evergreen.V3.Axis3d
import Evergreen.V3.Game
import Evergreen.V3.Guid
import Evergreen.V3.Physics.Coordinates
import Evergreen.V3.Point3d
import Evergreen.V3.Scene3d.Material
import Lamdera
import Length
import Time
import Url
import WebGL.Texture


type alias LoadingModel = 
    { ballTextures : (Dict.Dict Int (Evergreen.V3.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Maybe (Evergreen.V3.Scene3d.Material.Texture Float))
    , dimensions : (Maybe (Float, Float))
    }


type GenerateLinkStatus
    = LinkNotRequested
    | LinkGenerating
    | LinkGenerated Evergreen.V3.Guid.Guid


type Player
    = Player1
    | Player2


type PlayerAction
    = PlacedBallInHand (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates)
    | PlacedBallInKitchen (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates)
    | PlayerShot (Evergreen.V3.Axis3d.Axis3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates) Duration.Duration Evergreen.V3.Game.Camera
    | MovedCue (Maybe Evergreen.V3.Game.CueData) Evergreen.V3.Game.Camera


type FrontendStatus
    = Loading LoadingModel Url.Url
    | StartMenu GenerateLinkStatus Evergreen.V3.Game.Model
    | Running Evergreen.V3.Game.Model
    | NetworkPlay Evergreen.V3.Guid.Guid Evergreen.V3.Game.Model Player (List PlayerAction)
    | Failed String


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , status : FrontendStatus
    }


type alias PlayLink = 
    { owner : Lamdera.ClientId
    , sharedTo : (Maybe Lamdera.ClientId)
    }


type alias BackendModel =
    { message : String
    , links : (Dict.Dict String PlayLink)
    }


type FrontendMsg
    = GotInitialViewport Browser.Dom.Viewport
    | GotBallTexture Int (Result WebGL.Texture.Error (Evergreen.V3.Scene3d.Material.Texture Color.Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Evergreen.V3.Scene3d.Material.Texture Float))
    | RunningMsg Evergreen.V3.Game.Msg
    | MyPlayerRunningMsg Evergreen.V3.Game.Msg
    | NoOpFrontendMsg
    | LocalPlaySelected
    | NetworkPlaySelected
    | Tick Time.Posix
    | UrlChanged Url.Url
    | ClickedLink Browser.UrlRequest


type ToBackend
    = GenerateLink
    | PlayerJoined Evergreen.V3.Guid.Guid
    | PlayerAction Evergreen.V3.Guid.Guid PlayerAction


type BackendMsg
    = NoOpBackendMsg
    | RandomGeneratedLinkBackend Lamdera.ClientId Evergreen.V3.Guid.Guid


type ToFrontend
    = GeneratedLink Evergreen.V3.Guid.Guid
    | OtherPlayerJoined
    | OtherPlayerAction PlayerAction