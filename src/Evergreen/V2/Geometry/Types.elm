module Evergreen.V2.Geometry.Types exposing (..)

type Point2d units coordinates
    = Point2d 
    { x : Float
    , y : Float
    }


type Point3d units coordinates
    = Point3d 
    { x : Float
    , y : Float
    , z : Float
    }


type Direction3d coordinates
    = Direction3d 
    { x : Float
    , y : Float
    , z : Float
    }


type Axis3d units coordinates
    = Axis3d 
    { originPoint : (Point3d units coordinates)
    , direction : (Direction3d coordinates)
    }