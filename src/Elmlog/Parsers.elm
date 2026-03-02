module Elmlog.Parsers exposing
    ( emailNodeParser
    , emailParser
    , httpPrefixParser
    , isDomainChar
    , isEmailNameChar
    , isPathChar
    , isTLDChar
    , isWhitespaceChar
    , linkNodeParser
    , linkParser
    )

{-| Parsers.elm

Various Parser use.

-}

import Html exposing (Html, a, text)
import Html.Attributes exposing (href)
import Html.Parser exposing (Node(..))
import Parser exposing ((|.), (|=), DeadEnd, Parser)


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
        (char /= '/')
            && not
                (isWhitespaceChar <|
                    Debug.log "isTDLChar" char
                )


tldChars : Parser String
tldChars =
    Parser.chompWhile isTLDChar
        |> Parser.getChompedString


isPathChar : Char -> Bool
isPathChar char =
    not <| isWhitespaceChar char


pathChars : Parser String
pathChars =
    Parser.chompWhile isPathChar
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


emailNodeParser : Parser Node
emailNodeParser =
    emailParser
        |> Parser.andThen
            (\email ->
                let
                    emailString =
                        email.name ++ "@" ++ email.domain ++ "." ++ email.tld
                in
                Element "a"
                    [ ( "href", "mailto:" ++ emailString ) ]
                    [ Text emailString ]
                    |> Parser.succeed
            )


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


{-| [http(s)://]name.tld/path
-}
type alias Link =
    { connection : Maybe HTTP
    , name : String
    , tld : String
    , path : String
    }


linkToHtml : Link -> Html msg
linkToHtml { connection, name, tld, path } =
    let
        string =
            (case connection of
                Nothing ->
                    ""

                Just http ->
                    if http == Http then
                        "http://"

                    else
                        "https://"
            )
                ++ name
                ++ "."
                ++ tld
                ++ (if path == "" then
                        ""

                    else
                        "/" ++ path
                   )
    in
    a [ href string ]
        [ text string ]


linkParser : Parser Link
linkParser =
    Parser.succeed Link
        |= httpPrefixParser
        |. Parser.spaces
        |= domainChars
        |. Parser.symbol "."
        |= tldChars
        |= Parser.oneOf
            [ Parser.symbol "/"
                |> Parser.andThen
                    (\_ -> pathChars)
            , Parser.succeed ""
            ]


linkNodeParser : Parser Node
linkNodeParser =
    linkParser
        |> Parser.andThen
            (\link ->
                let
                    linkString =
                        (case link.connection of
                            Nothing ->
                                ""

                            Just http ->
                                case http of
                                    Http ->
                                        "http://"

                                    Https ->
                                        "https://"
                        )
                            ++ link.name
                            ++ "."
                            ++ link.tld
                            ++ (if link.path == "" then
                                    ""

                                else
                                    "/" ++ link.path
                               )
                in
                Parser.succeed <|
                    Element "a"
                        [ ( "href", linkString ) ]
                        [ Text linkString ]
            )


mapResults : (Node -> Result err Node) -> List Node -> Result err (List Node)
mapResults mapper nodes =
    let
        mapit : List Node -> List Node -> Result err (List Node)
        mapit result otherNodes =
            case otherNodes of
                [] ->
                    Ok <| List.reverse result

                node :: rest ->
                    case mapper node of
                        Err err ->
                            Err err

                        Ok mapped ->
                            mapit (mapped :: result) rest
    in
    mapit [] nodes


mapNodes : String -> (String -> Result (List DeadEnd) (List Node)) -> Result (List DeadEnd) (List Node)
mapNodes string mapper =
    case Html.Parser.run string of
        Err deadends ->
            Err deadends

        Ok nodes ->
            let
                eachNode : Node -> Result (List DeadEnd) Node
                eachNode node =
                    case node of
                        Comment _ ->
                            Ok node

                        Text text ->
                            -- TODO
                            case mapper text of
                                Err deadends ->
                                    Err deadends

                                Ok mappedNodes ->
                                    Ok
                                        (case mappedNodes of
                                            [] ->
                                                Text ""

                                            [ n ] ->
                                                n

                                            _ ->
                                                Element "span" [] mappedNodes
                                        )

                        Element name attributes subnodes ->
                            if name == "a" then
                                Ok node

                            else
                                case mapResults eachNode subnodes of
                                    Err deadends ->
                                        Err deadends

                                    Ok mappedNodes ->
                                        Ok <| Element name attributes mappedNodes
            in
            mapResults eachNode nodes


extractParsed : String -> Parser Node -> (String -> ( String, String )) -> Result (List DeadEnd) (List Node)
extractParsed string parser scanToSeparator =
    let
        extractInternal : String -> String -> Result (List DeadEnd) (List Node)
        extractInternal prefix suffix =
            case Parser.run parser suffix of
                Err deadends ->
                    if suffix == "" then
                        Err deadends

                    else
                        let
                            ( head, tail ) =
                                scanToSeparator suffix
                        in
                        if tail == "" then
                            Err deadends

                        else
                            extractInternal (prefix ++ head) tail

                Ok node ->
                    if prefix == "" then
                        Ok [ node ]

                    else
                        Ok [ Text prefix, node ]
    in
    extractInternal "" string
