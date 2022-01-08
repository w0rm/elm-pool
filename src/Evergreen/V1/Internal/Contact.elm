module Evergreen.V1.Internal.Contact exposing (..)

import Evergreen.V1.Internal.Body
import Evergreen.V1.Internal.Vector3


type alias Contact = 
    { ni : Evergreen.V1.Internal.Vector3.Vec3
    , pi : Evergreen.V1.Internal.Vector3.Vec3
    , pj : Evergreen.V1.Internal.Vector3.Vec3
    }


type alias ContactGroup data = 
    { body1 : (Evergreen.V1.Internal.Body.Body data)
    , body2 : (Evergreen.V1.Internal.Body.Body data)
    , contacts : (List Contact)
    }