---------------------------------------------------------------
--
-- Main.elm
-- Weblog, a website to help maintain a weblog.
-- Copyright (c) 2026 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (main)

{-| Weblog.org top-level

Weblog is a template wrapper that helps in creating a weblog.
A weblog is a frequently updated web site, usually organized by date.
It is the precursor to today's social media, without the barriers.

-}

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Elmlog.Types exposing (InputType(..))
import Html exposing (Html, a, div, fieldset, img, input, legend, p, span, text, textarea, ul)
import Html.Attributes exposing (checked, disabled, href, name, src, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Parser exposing (Node(..), run)
import Html.Parser.Util exposing (toVirtualDom)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra
import Markdown
import Parser exposing ((|.), DeadEnd, Parser)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        , subscriptions = \model -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { url : Url
    , key : Key
    , userText : String
    , preview : Html Msg
    , error : Maybe (List DeadEnd)
    , inputType : InputType
    , showEditor : Bool
    }


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | InputTextArea String
    | SetInputType InputType
    | ToggleShowEditor


defaultInputType : InputType
defaultInputType =
    FilteredHtmlInput


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        userText =
            """
<b>bold</b> <i>italic</i> <b><i>both</i></b>
And a broken line.

New paragraph <i>italic</i> considered OK.

<img src='images/elmlog-screenshot-260218.jpg' width='200'>
    """

        --     "<b>bold</b> <i>italic</i> <b><i>both</i></b>"
        -- defaultInputType = MarkdownInput
        -- "Hello, World.\n\n**bold text.** _Italic text._ **_both._**\n\n[billstclair.com](https://billstclair.com/)\n\n![Mastodon](https://mammudeck.com/images/icon-192.png)"
        ( preview, maybeDeadends ) =
            toHtml userText defaultInputType
    in
    { url = url
    , key = key
    , userText = userText
    , preview = preview
    , error = maybeDeadends
    , inputType = defaultInputType
    , showEditor = True
    }
        |> withNoCmd


h2 : String -> Html Msg
h2 string =
    Html.h2 [] [ text string ]


h4 : String -> Html Msg
h4 string =
    Html.h4 [] [ text string ]


inputTypeRadioName : String
inputTypeRadioName =
    "inputTypeRadioName"


view : Model -> Document Msg
view model =
    { title = "Elmlog"
    , body =
        [ div
            [ style "margin" "10px"
            , style "overflow" "auto"
            , style "height" "90%"
            ]
            [ Html.h2 []
                [ img
                    [ src "images/icon-180.png"
                    , width 50
                    ]
                    []
                , text " Elmlog"
                ]
            , p []
                [ model.preview ]
            , if not model.showEditor then
                a
                    [ href "#"
                    , onClick ToggleShowEditor
                    ]
                    [ text "-- show editor --" ]

              else
                div []
                    [ a
                        [ href "#"
                        , onClick ToggleShowEditor
                        ]
                        [ text "-- hide editor --" ]
                    , editor model
                    , p []
                        [ a [ href "https://github.com/billstclair/elmlog" ]
                            [ img
                                [ src "images/GitHub-Mark-32px.png"
                                ]
                                []
                            , text " GitHub"
                            ]
                        , br
                        , a [ href "https://github.com/billstclair/elmlog/blob/main/LICENSE" ]
                            [ text "© Copyright" ]
                        , text " 2026, Bill St. Clair"
                        ]
                    ]
            ]
        ]
    }


editor : Model -> Html Msg
editor model =
    span []
        [ p
            [ style "margin" "10px" ]
            [ textarea
                [ style "width" "50em"
                , style "height" "10em"
                , onInput InputTextArea
                ]
                [ text model.userText ]
            , case model.error of
                Nothing ->
                    text ""

                Just deadends ->
                    div []
                        [ h4 "Error"
                        , text <| deadEndsToString deadends
                        , br
                        , br
                        ]
            , fieldset []
                [ radioButton
                    { buttonValue = MarkdownInput
                    , radioValue = model.inputType
                    , radioName = "input-type"
                    , setter = SetInputType
                    , label = "Markdown"
                    }
                , br
                , radioButton
                    { buttonValue = FilteredHtmlInput
                    , radioValue = model.inputType
                    , radioName = "input-type"
                    , setter = SetInputType
                    , label = "Filtered Html"
                    }
                , bullets
                    [ li "convert line and paragraph breaks to <br> & <p>"
                    , li "convert URLs and email address to links"
                    , li "allowed HTML tags: <a> <em> <strong> <cite> <code> <ul> <ol> <li> <dl> <dt> <dd> <i> <b> <u> <blockquote> <pre>"
                    ]
                , radioButton
                    { buttonValue = FullHtmlInput
                    , radioValue = model.inputType
                    , radioName = "input-type"
                    , setter = SetInputType
                    , label = "Full HTML"
                    }
                , bullets
                    [ li "convert line and paragraph breaks to <br> & <p>"
                    , li "convert URLs and email address to links"
                    ]
                , radioButton
                    { buttonValue = RawHtmlInput
                    , radioValue = model.inputType
                    , radioName = "input-type"
                    , setter = SetInputType
                    , label = "Raw HTML"
                    }
                ]
            ]
        ]


bullets : List (Html msg) -> Html msg
bullets elements =
    Html.ul
        [ style "margin-top" "0"
        , style "margin-bottom" "0"
        ]
        elements


li : String -> Html msg
li string =
    Html.li [] [ text string ]


br : Html msg
br =
    Html.br [] []


radioButton : { buttonValue : a, radioValue : a, radioName : String, setter : a -> Msg, label : String } -> Html Msg
radioButton { buttonValue, radioValue, radioName, setter, label } =
    span []
        [ input
            [ type_ "radio"
            , name radioName
            , value label
            , checked <| buttonValue == radioValue
            , onCheck <|
                \checked ->
                    setter buttonValue
            ]
            []
        , span
            [ onClick <| setter buttonValue
            , style "cursor" "default"
            ]
            [ text label ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        InputTextArea string ->
            let
                ( preview, error ) =
                    toHtml string model.inputType
            in
            { model
                | userText = string
                , preview = preview
                , error = error
            }
                |> withNoCmd

        SetInputType inputType ->
            let
                ( preview, error ) =
                    toHtml model.userText inputType
            in
            ( Debug.log
                ("SetInputType \"" ++ Debug.toString inputType ++ "\", model")
                { model
                    | inputType = inputType
                    , preview = preview
                    , error = error
                }
            , Cmd.none
            )

        ToggleShowEditor ->
            { model
                | showEditor = not model.showEditor
            }
                |> withNoCmd

        OnUrlRequest request ->
            case request of
                Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                External string ->
                    ( model
                    , Navigation.load string
                    )

        OnUrlChange url ->
            ( model
            , Cmd.none
              --            , Navigation.pushUrl model.key <|
              --                Debug.log "  " (Url.toString <| Debug.log "OnUrlChange" url)
            )


toHtml : String -> InputType -> ( Html Msg, Maybe (List DeadEnd) )
toHtml string inputType =
    case inputType of
        MarkdownInput ->
            ( Markdown.toHtml [] string
            , Nothing
            )

        _ ->
            parseHtml string inputType


{-| The <elmlog>...</elmlog> tag does nothing. It isn't even defined.
-}
elmlog : String -> Html msg
elmlog string =
    Html.node "elmlog"
        []
        [ text string ]


{-| The first return value is error message if there was one.
The second return value is either the original string or a filtered version.
-}
parseHtml : String -> InputType -> ( Html Msg, Maybe (List DeadEnd) )
parseHtml string inputType =
    case Html.Parser.run string of
        Err deadEnds ->
            ( elmlog string
            , Just deadEnds
            )

        Ok nodes ->
            let
                filteredNodes =
                    case inputType of
                        FilteredHtmlInput ->
                            addPsAndBRs nodes
                                |> filteredHtmlNodes
                                |> emailAndWebsitesToLinks

                        FullHtmlInput ->
                            addPsAndBRs nodes
                                |> emailAndWebsitesToLinks

                        RawHtmlInput ->
                            nodes

                        _ ->
                            nodes
            in
            ( span [] <|
                toVirtualDom filteredNodes
            , Nothing
            )


emailAndWebsitesToLinks : List Html.Parser.Node -> List Html.Parser.Node
emailAndWebsitesToLinks nodes =
    List.concat <| List.map emailOrWebsiteToLinks nodes


emailOrWebsiteToLinks : Html.Parser.Node -> List Html.Parser.Node
emailOrWebsiteToLinks node =
    case node of
        Text s ->
            emailToLinks s
                |> List.map
                    (\n ->
                        case n of
                            Text ss ->
                                websiteToLinks ss

                            Element name attribs subnodes ->
                                [ Element name
                                    attribs
                                    (List.map emailOrWebsiteToLinks subnodes
                                        |> List.concat
                                    )
                                ]

                            _ ->
                                [ n ]
                    )
                |> List.concat

        _ ->
            [ node ]


emailParser : Parser Html.Parser.Node
emailParser =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isAlphaNum c)
            |. Parser.chompIf (\c -> c == '@')
            |. Parser.chompIf (\c -> Char.isAlphaNum c)
            |. Parser.chompIf (\c -> c == '.')
            |. Parser.chompIf (\c -> Char.isAlphaNum c)
        )
        |> Parser.andThen
            (\email ->
                Parser.succeed
                    (Element "a"
                        [ ( "href", "mailto:" ++ email ) ]
                        [ Text email ]
                    )
            )


parseEmail : String -> Result (List Parser.DeadEnd) Html.Parser.Node
parseEmail string =
    Parser.run emailParser string


emailToLinks : String -> List Html.Parser.Node
emailToLinks string =
    [ Text string ]


websiteToLinks : String -> List Html.Parser.Node
websiteToLinks string =
    [ Text string ]


addPsAndBRs : List Html.Parser.Node -> List Html.Parser.Node
addPsAndBRs nodes =
    -- TODO
    addBRs <| addPs nodes


addPs : List Html.Parser.Node -> List Html.Parser.Node
addPs nodes =
    let
        loop : List Html.Parser.Node -> List Html.Parser.Node -> List Html.Parser.Node -> List Html.Parser.Node
        -- prev is reversed.
        -- para is the current paragraph.
        loop prev para list =
            case list of
                [] ->
                    List.append (List.reverse prev) para

                node :: rest ->
                    case node of
                        Text string ->
                            case String.split "\n\n" string of
                                [] ->
                                    Debug.todo "This can't happen."

                                [ s ] ->
                                    loop prev
                                        (List.append para [ Text s ])
                                        rest

                                s :: srest ->
                                    let
                                        maybeSlast =
                                            car (List.reverse srest)

                                        sButlast =
                                            List.reverse (cdr <| List.reverse srest)

                                        paras =
                                            List.map (\ss -> Element "p" [] [ Text ss ]) sButlast
                                    in
                                    loop
                                        ((Element "p" [] <|
                                            List.concat [ para, [ Text s ] ]
                                         )
                                            :: (List.reverse paras ++ prev)
                                        )
                                        [ case maybeSlast of
                                            Nothing ->
                                                Comment "Can't happen"

                                            Just slast ->
                                                Text slast
                                        ]
                                        rest

                        Element name attribs subnodes ->
                            if name == "p" then
                                loop
                                    (List.concat
                                        [ [ node, Element "p" [] para ]
                                        , prev
                                        ]
                                    )
                                    []
                                    rest

                            else
                                loop
                                    prev
                                    (List.append
                                        para
                                        [ Element name
                                            attribs
                                          <|
                                            addPs subnodes
                                        ]
                                    )
                                    rest

                        Comment _ ->
                            loop prev
                                (List.concat [ para, [ node ] ])
                                rest
    in
    loop [] [] nodes


car : List x -> Maybe x
car cons =
    List.head cons


cdr : List x -> List x
cdr cons =
    case List.tail cons of
        Nothing ->
            []

        Just tail ->
            tail


isText : Html.Parser.Node -> Bool
isText node =
    case node of
        Text _ ->
            True

        _ ->
            False


addBRs : List Html.Parser.Node -> List Html.Parser.Node
addBRs nodes =
    List.map nLtoBR nodes


nLtoBR : Html.Parser.Node -> Html.Parser.Node
nLtoBR node =
    case node of
        Text s ->
            case String.split "\n" s of
                [] ->
                    node

                [ _ ] ->
                    node

                ss ->
                    Element "span"
                        []
                    <|
                        (List.map Text ss
                            |> List.intersperse brElement
                        )

        Element name attributes subnodes ->
            Element name attributes <| List.map nLtoBR subnodes

        comment ->
            comment


brElement : Html.Parser.Node
brElement =
    Element "br" [] []


allowedHtmlNames : List String
allowedHtmlNames =
    [ "a"
    , "em"
    , "strong"
    , "cite"
    , "code"
    , "ul"
    , "ol"
    , "li"
    , "dl"
    , "dt"
    , "dd"
    , "i"
    , "b"
    , "u"
    , "blockquote"
    , "pre"
    , "p"
    , "br"
    , "span"
    ]


filteredHtmlNodes : List Html.Parser.Node -> List Html.Parser.Node
filteredHtmlNodes nodes =
    List.map filterNode nodes


printAttributes : List Html.Parser.Attribute -> String
printAttributes attributes =
    List.map printAttrib attributes
        |> List.intersperse " "
        |> String.concat


printAttrib : Html.Parser.Attribute -> String
printAttrib ( name, value ) =
    " " ++ name ++ "=" ++ "'" ++ escapeSingleQuotes value ++ "'"


escapeSingleQuotes : String -> String
escapeSingleQuotes string =
    String.replace "'" "\\'" string


filterNode : Html.Parser.Node -> Html.Parser.Node
filterNode node =
    case node of
        Element name attribs subnodes ->
            if List.member name allowedHtmlNames then
                Element name attribs <| filteredHtmlNodes subnodes

            else
                Element "span"
                    []
                    (Text ("<" ++ name ++ printAttributes attribs ++ ">")
                        :: (filteredHtmlNodes subnodes
                                ++ [ Text ("</" ++ name ++ ">") ]
                           )
                    )

        _ ->
            node


nodesToString : List Html.Parser.Node -> String
nodesToString nodes =
    List.map Html.Parser.nodeToString nodes
        |> String.concat



-- Parser.deadendsToString has not yet been implemented.


deadEndsToString : deadends -> String
deadEndsToString deadends =
    Debug.toString deadends
