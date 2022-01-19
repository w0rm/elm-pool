module Route exposing
    ( Route(..)
    , fromUrl
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

import Guid exposing (Guid)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser, oneOf, parse, s)


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
