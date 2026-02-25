module Elmlog.Parsers exposing (emailParser)

{-| Parsers.elm

Various Parser use.

-}

import Parser exposing ((|.), (|=), Parser)


isEmailNameChar : Char -> Bool
isEmailNameChar char =
    (not <| isWhitespaceChar char)
        && (char /= '@')


isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    (char == '\n')
        || (char == ' ')
        || (char == '\t')


isDomainChar : Char -> Bool
isDomainChar char =
    (not <| isWhitespaceChar char)
        && (char /= '.')


isTLDChar : Char -> Bool
isTLDChar char =
    not <| isWhitespaceChar char


{-| emailParser
Parses an email address, preceded by whatever.
The second value in the returned pair is the email address.
-}
emailParser : Parser ( String, String )
emailParser =
    let
        justEmail : Parser String
        justEmail =
            Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\c -> isEmailNameChar c)
                    |. Parser.chompIf (\c -> c == '@')
                    |. Parser.chompIf (\c -> isDomainChar c)
                    |. Parser.chompIf (\c -> c == '.')
                    |. Parser.chompIf (\c -> isTLDChar c)
                )
    in
    justEmail
        |> Parser.andThen
            (\x -> Parser.succeed ( "", x ))
