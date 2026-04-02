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
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))


isEmailNameChar : Char -> Bool
isEmailNameChar char =
    (not <| isWhitespaceChar char)
        && (char /= '@')


emailNameChars : Parser String
emailNameChars =
    Parser.chompWhile isEmailNameChar
        |> Parser.getChompedString


isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    (char == '\n')
        || (char == ' ')
        || (char == '\t')


whitespaceChars : Parser String
whitespaceChars =
    Parser.chompWhile isWhitespaceChar
        |> Parser.getChompedString


isDomainChar : Char -> Bool
isDomainChar char =
    (not <| isWhitespaceChar char)
        && (char /= '.')
        && (char /= '/')


domainChars : Parser String
domainChars =
    Parser.chompWhile isDomainChar
        |> Parser.getChompedString


isTLDChar : Char -> Bool
isTLDChar char =
    (char /= '/')
        && not
            (isWhitespaceChar char)


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
        [ Parser.symbol "https://"
            |> Parser.andThen
                (\_ -> Parser.succeed <| Just Https)
        , Parser.symbol "http://"
            |> Parser.andThen
                (\_ -> Parser.succeed <| Just Http)
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


mapResults : (node -> Result err node) -> List node -> Result err (List node)
mapResults mapper nodes =
    let
        mapit : List node -> List node -> Result err (List node)
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


scanTo : String -> (String -> Bool) -> Int -> ( String, String )
scanTo string predicate index =
    let
        len =
            String.length string

        scanInternal idx =
            if idx >= len then
                ( string, "" )

            else
                let
                    ( head, tail ) =
                        ( String.left idx string, String.dropLeft idx string )
                in
                if predicate <| String.left 1 tail then
                    ( head, tail )

                else
                    scanInternal <| index + 1
    in
    scanInternal index


parseOne : Parser n -> String -> (String -> ( String, String )) -> ( String, Node, String )
parseOne parser input scanner =
    case runFront parser input of
        Err deadends ->
            ( "", Text input, "" )

        Ok ( node, suffix ) ->
            ( "", node, suffix )


runFront : Parser n -> String -> Result Parser.DeadEnd ( Node, String )
runFront parser string =
    Err
        { row = 1
        , col = 1
        , problem = Problem "TODO"
        }


parseAll : String -> Parser n -> List Node
parseAll string parser =
    -- TODO
    []


scanToEmailSeparator : String -> ( String, String )
scanToEmailSeparator string =
    let
        predicate : String -> Bool
        predicate s =
            case String.uncons s of
                Nothing ->
                    False

                Just ( c, _ ) ->
                    not <| isWhitespaceChar c
    in
    scanTo string predicate 1


scanToHtmlSeparator : String -> ( String, String )
scanToHtmlSeparator string =
    let
        predicate : String -> Bool
        predicate s =
            case String.uncons s of
                Nothing ->
                    False

                Just ( c, _ ) ->
                    not <| isWhitespaceChar c
    in
    scanTo string predicate 1
