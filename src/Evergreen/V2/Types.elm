module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Color
import Dict
import Duration
import Evergreen.V2.Axis3d
import Evergreen.V2.Game
import Evergreen.V2.Guid
import Evergreen.V2.Physics.Coordinates
import Evergreen.V2.Point3d
import Evergreen.V2.Scene3d.Material
import Lamdera
import Length
import Time
import Url
import WebGL.Texture


type alias LoadingModel = 
    { ballTextures : (Dict.Dict Int (Evergreen.V2.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Maybe (Evergreen.V2.Scene3d.Material.Texture Float))
    , dimensions : (Maybe (Float, Float))
    }


type GenerateLinkStatus
    = LinkNotRequested
    | LinkGenerating
    | LinkGenerated Evergreen.V2.Guid.Guid


type Player
    = Player1
    | Player2


type PlayerAction
    = PlacedBallInHand (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates)
    | PlacedBallInKitchen (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates)
    | PlayerShot (Evergreen.V2.Axis3d.Axis3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates) Duration.Duration Evergreen.V2.Game.Camera
    | MovedCue (Maybe Evergreen.V2.Game.CueData) Evergreen.V2.Game.Camera


type FrontendStatus
    = Loading LoadingModel Url.Url
    | StartMenu GenerateLinkStatus Evergreen.V2.Game.Model
    | Running Evergreen.V2.Game.Model
    | NetworkPlay Evergreen.V2.Guid.Guid Evergreen.V2.Game.Model Player (List PlayerAction)
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
    | GotBallTexture Int (Result WebGL.Texture.Error (Evergreen.V2.Scene3d.Material.Texture Color.Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Evergreen.V2.Scene3d.Material.Texture Float))
    | RunningMsg Evergreen.V2.Game.Msg
    | MyPlayerRunningMsg Evergreen.V2.Game.Msg
    | NoOpFrontendMsg
    | LocalPlaySelected
    | NetworkPlaySelected
    | Tick Time.Posix
    | UrlChanged Url.Url
    | ClickedLink Browser.UrlRequest


type ToBackend
    = GenerateLink
    | PlayerJoined Evergreen.V2.Guid.Guid
    | PlayerAction Evergreen.V2.Guid.Guid PlayerAction


type BackendMsg
    = NoOpBackendMsg
    | RandomGeneratedLinkBackend Lamdera.ClientId Evergreen.V2.Guid.Guid


type ToFrontend
    = GeneratedLink Evergreen.V2.Guid.Guid
    | OtherPlayerJoined
    | OtherPlayerAction PlayerAction