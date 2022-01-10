module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Color
import Dict
import Duration
import Evergreen.V1.Axis3d
import Evergreen.V1.Game
import Evergreen.V1.Guid
import Evergreen.V1.Physics.Coordinates
import Evergreen.V1.Point3d
import Evergreen.V1.Scene3d.Material
import Lamdera
import Length
import Url
import WebGL.Texture


type alias LoadingModel = 
    { ballTextures : (Dict.Dict Int (Evergreen.V1.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Maybe (Evergreen.V1.Scene3d.Material.Texture Float))
    , dimensions : (Maybe (Float, Float))
    }


type GenerateLinkStatus
    = LinkNotRequested
    | LinkGenerating
    | LinkGenerated Evergreen.V1.Guid.Guid


type Player
    = Player1
    | Player2


type FrontendStatus
    = Loading LoadingModel Url.Url
    | StartMenu GenerateLinkStatus Evergreen.V1.Game.Model
    | Running Evergreen.V1.Game.Model
    | NetworkPlay Evergreen.V1.Guid.Guid Evergreen.V1.Game.Model Player
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
    | GotBallTexture Int (Result WebGL.Texture.Error (Evergreen.V1.Scene3d.Material.Texture Color.Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Evergreen.V1.Scene3d.Material.Texture Float))
    | RunningMsg Evergreen.V1.Game.Msg
    | MyPlayerRunningMsg Evergreen.V1.Game.Msg
    | NoOpFrontendMsg
    | LocalPlaySelected
    | NetworkPlaySelected
    | UrlChanged Url.Url
    | ClickedLink Browser.UrlRequest


type PlayerAction
    = PlacedBallInHand (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates)
    | PlacedBallInKitchen (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates)
    | PlayerShot (Evergreen.V1.Axis3d.Axis3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates) Duration.Duration
    | MovedCue (Maybe Evergreen.V1.Game.CueData) Evergreen.V1.Game.Camera


type ToBackend
    = GenerateLink
    | PlayerJoined Evergreen.V1.Guid.Guid
    | PlayerAction Evergreen.V1.Guid.Guid PlayerAction


type BackendMsg
    = NoOpBackendMsg
    | RandomGeneratedLinkBackend Lamdera.ClientId Evergreen.V1.Guid.Guid


type ToFrontend
    = GeneratedLink Evergreen.V1.Guid.Guid
    | OtherPlayerJoined
    | OtherPlayerAction PlayerAction