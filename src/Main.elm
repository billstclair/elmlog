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
import Elmlog.Types exposing (Message(..), MessageType(..), messageText)
import Html exposing (Html, a, div, img, p, text, textarea)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Markdown exposing (toHtml)
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
    , messageType : MessageType
    , message : Message
    , preview : Html Msg
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | InputTextArea String


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        message =
            MarkdownMessage "Hello, World.\n\n**bold text.** _Italic text._ **_both._**\n\n[billstclair.com](https://billstclair.com/)\n\n![Mastodon](https://mammudeck.com/images/icon-192.png)"
    in
    { url = url
    , key = key
    , messageType = MarkdownType
    , message = message
    , preview = toHtml message
    }
        |> withNoCmd


h2 : String -> Html Msg
h2 string =
    Html.h2 [] [ text string ]


h4 : String -> Html Msg
h4 string =
    Html.h3 [] [ text string ]


view : Model -> Document Msg
view model =
    { title = "Elmlog"
    , body =
        [ h2 "Elmlog"
        , div
            [ style "margin" "10px"
            , style "overflow" "auto"
            , style "height" "90%"
            ]
            [ h4 "Content"
            , p
                [ style "margin" "10px"
                , onInput InputTextArea
                ]
                [ textarea
                    [ style "width" "50em"
                    , style "height" "20em"
                    ]
                    [ text <| messageText model.message ]
                ]
            , h4 "Preview"
            , p []
                [ model.preview ]
            , p []
                [ a [ href "https://github.com/billstclair/elmlog" ]
                    [ img
                        [ src "images/GitHub-Mark-32px.png"
                        ]
                        []
                    ]
                ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTextArea string ->
            let
                message =
                    MarkdownMessage string
            in
            { model
                | message = message
                , preview = toHtml message
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
            , Navigation.pushUrl model.key (Url.toString url)
            )


toHtml : Message -> Html Msg
toHtml message =
    case message of
        TextMessage string ->
            text string

        FilteredHtmlMessage string ->
            parseHtml string FilteredType

        PlainHtmlMessage string ->
            parseHtml string PlainType

        RawHtmlMessage string ->
            parseHtml string RawType

        MarkdownMessage string ->
            Markdown.toHtml [] string


parseHtml : String -> MessageType -> Html Msg
parseHtml string messageType =
    -- TODO
    text string
