module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule
        [ { moduleName = "Point2d"
          , typeName = "Point2d"
          , index = 1 -- Position of the phantom variable in the type's arguments
          }
        ]
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
        |> Review.Rule.ignoreErrorsForFiles
            [ "src/Env.elm" -- This file is required for Lamdera.
            ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule
    ]
        |> List.map
            (Review.Rule.ignoreErrorsForDirectories
                [ "vendor/"
                , "src/Evergreen/" -- These are generated by Lamdera.
                ]
            )
