module Elmlog.Parsers exposing (emailParser, isDomainChar, isEmailNameChar, isTLDChar, isWhitespaceChar)

{-| Parsers.elm

Various Parser use.

-}

import Parser exposing ((|.), (|=), Parser)


isEmailNameChar : Char -> Bool
isEmailNameChar char =
    Debug.log "  " <|
        (not <| isWhitespaceChar <| Debug.log "isEmailNameChar" char)
            && (char /= '@')


isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    Debug.log "  " <|
        (Debug.log "isWhiteSpaceChar" char == '\n')
            || (char == ' ')
            || (char == '\t')


isDomainChar : Char -> Bool
isDomainChar char =
    Debug.log "  " <|
        (not <| isWhitespaceChar <| Debug.log "isDomainChar" char)
            && (char /= '.')


isTLDChar : Char -> Bool
isTLDChar char =
    Debug.log "  " <|
        not <|
            isWhitespaceChar <|
                Debug.log "isTDLChar" char


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
