module Elmlog.Parsers exposing
    ( emailParser
    , httpPrefixParser
    , isDomainChar
    , isEmailNameChar
    , isTLDChar
    , isWhitespaceChar
    , linkParser
    )

{-| Parsers.elm

Various Parser use.

-}

import Parser exposing ((|.), (|=), Parser)


isEmailNameChar : Char -> Bool
isEmailNameChar char =
    Debug.log "  " <|
        (not <| isWhitespaceChar <| Debug.log "isEmailNameChar" char)
            && (char /= '@')


emailNameChars : Parser String
emailNameChars =
    Parser.chompWhile isEmailNameChar
        |> Parser.getChompedString


isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    Debug.log "  " <|
        (Debug.log "isWhiteSpaceChar" char == '\n')
            || (char == ' ')
            || (char == '\t')


whitespaceChars : Parser String
whitespaceChars =
    Parser.chompWhile isWhitespaceChar
        |> Parser.getChompedString


isDomainChar : Char -> Bool
isDomainChar char =
    Debug.log "  " <|
        (not <| isWhitespaceChar <| Debug.log "isDomainChar" char)
            && (char /= '.')


domainChars : Parser String
domainChars =
    Parser.chompWhile isDomainChar
        |> Parser.getChompedString


isTLDChar : Char -> Bool
isTLDChar char =
    Debug.log "  " <|
        not <|
            isWhitespaceChar <|
                Debug.log "isTDLChar" char


tldChars : Parser String
tldChars =
    Parser.chompWhile isTLDChar
        |> Parser.getChompedString


type alias Email =
    { name : String
    , domain : String
    , tld : String
    }


{-| emailParser
Parses an email address, preceded by whatever.
The second value in the returned pair is the email address.
-}
emailParser : Parser Email
emailParser =
    Parser.succeed Email
        |= emailNameChars
        |. Parser.symbol "@"
        |= domainChars
        |. Parser.symbol "."
        |= tldChars


type HTTP
    = Http
    | Https


httpPrefixParser : Parser (Maybe HTTP)
httpPrefixParser =
    Parser.oneOf
        [ Parser.symbol "http"
            |> Parser.andThen
                (\_ ->
                    Parser.oneOf
                        [ Parser.symbol "s://"
                            |> Parser.andThen
                                (\_ -> Parser.succeed <| Just Https)
                        , Parser.symbol "://"
                            |> Parser.andThen
                                (\_ -> Parser.succeed <| Just Http)
                        , Parser.succeed Nothing
                        ]
                )
        , Parser.succeed Nothing
        ]


type alias Link =
    { connection : Maybe HTTP
    , name : String
    , tld : String
    }


linkParser : Parser Link
linkParser =
    -- TODO
    Parser.succeed
        { connection = Nothing
        , name = "sample"
        , tld = "com"
        }
