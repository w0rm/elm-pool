module Route exposing
    ( Route(..)
    , fromUrl, toString
    , to
    )

{-| Routing for the app.


# Basics

@docs Route


# Helpers

@docs fromUrl, toString


# Routing to...

@docs to


## Home

@docs toHome

-}

import Browser.Navigation as Navigation
import Dict
import Guid exposing (Guid)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Url exposing ((</>), (<?>), Parser, oneOf, parse, s)
import Url.Parser.Query


type Route
    = Home
    | Play Guid


fromUrl : Url -> Maybe Route
fromUrl url =
    parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Url.map Home Url.top
        , Url.map Play (s "play" </> Guid.parser)
        ]


to : Route -> Navigation.Key -> Cmd msg
to route navKey =
    Navigation.pushUrl navKey (routeToUrl route)


toHome : Navigation.Key -> Cmd msg
toHome =
    to Home


toString : Route -> String
toString =
    routeToUrl


routeToUrl : Route -> String
routeToUrl route =
    "#/"
        ++ routeToRelativeUrl route


routeToRelativeUrl : Route -> String
routeToRelativeUrl route =
    case route of
        Home ->
            Url.Builder.relative
                [ "home"
                ]
                []

        Play guid ->
            Url.Builder.relative
                [ "play"
                , Guid.toString guid
                ]
                []
