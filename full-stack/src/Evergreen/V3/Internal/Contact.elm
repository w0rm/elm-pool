module Evergreen.V3.Internal.Contact exposing (..)

import Evergreen.V3.Internal.Body
import Evergreen.V3.Internal.Vector3


type alias Contact = 
    { ni : Evergreen.V3.Internal.Vector3.Vec3
    , pi : Evergreen.V3.Internal.Vector3.Vec3
    , pj : Evergreen.V3.Internal.Vector3.Vec3
    }


type alias ContactGroup data = 
    { body1 : (Evergreen.V3.Internal.Body.Body data)
    , body2 : (Evergreen.V3.Internal.Body.Body data)
    , contacts : (List Contact)
    }