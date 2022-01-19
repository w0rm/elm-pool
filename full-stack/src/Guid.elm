module Guid exposing (Guid(..), generator, parser, toString)

import Random exposing (Generator)
import Random.Char
import Random.Extra
import Random.String
import Url.Parser exposing (Parser)


type Guid
    = Guid String


generator : Generator Guid
generator =
    Random.map4
        (\part1 part2 part3 part4 ->
            part1 ++ "-" ++ part2 ++ "-" ++ part3 ++ "-" ++ part4
        )
        (alphaNumericGenerator 8)
        (alphaNumericGenerator 4)
        (alphaNumericGenerator 4)
        (alphaNumericGenerator 12)
        |> Random.map Guid


{-| From <https://github.com/elm/random/issues/2#issuecomment-813163905>.
-}
alphaNumericGenerator : Int -> Generator String
alphaNumericGenerator strLength =
    Random.Extra.choices (Random.Char.char 48 57)
        [ Random.Char.char 97 122
        , Random.Char.char 65 90
        ]
        |> Random.String.string strLength


parser : Parser (Guid -> a) a
parser =
    Url.Parser.custom "GUID" (\str -> Just (Guid str))


toString : Guid -> String
toString (Guid guid) =
    guid
