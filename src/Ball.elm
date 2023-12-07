module Ball exposing (body, radius, sphere)

import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Material as Material exposing (Material)
import Sphere3d exposing (Sphere3d)


radius : Length
radius =
    Length.millimeters (57.15 / 2)


weight : Mass
weight =
    Mass.grams 170


{-| The perfect sphere keeps rolling on the perfect plane,
we need to damp the angular velocity to make it stop.
-}
damping : { linear : Float, angular : Float }
damping =
    { linear = 0.4, angular = 0.4 }


ballMaterial : Material
ballMaterial =
    Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


sphere : Sphere3d Meters BodyCoordinates
sphere =
    Sphere3d.atOrigin radius


body : id -> Body id
body id =
    Body.sphere sphere
        id
        |> Body.withMaterial ballMaterial
        |> Body.withDamping damping
        |> Body.withBehavior (Body.dynamic weight)
