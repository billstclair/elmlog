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
import Html.Attributes exposing (checked, href, name, src, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Markdown
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
    , inputType : InputType
    }


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | InputTextArea String
    | SetInputType InputType


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        userText =
            "Hello, World.\n\n**bold text.** _Italic text._ **_both._**\n\n[billstclair.com](https://billstclair.com/)\n\n![Mastodon](https://mammudeck.com/images/icon-192.png)"
    in
    { url = url
    , key = key
    , userText = userText
    , preview = toHtml userText MarkdownInput
    , inputType = MarkdownInput
    }
        |> withNoCmd


h2 : String -> Html Msg
h2 string =
    Html.h2 [] [ text string ]


h4 : String -> Html Msg
h4 string =
    Html.h3 [] [ text string ]


inputTypeRadioName : String
inputTypeRadioName =
    "inputTypeRadioName"


view : Model -> Document Msg
view model =
    { title = "Elmlog"
    , body =
        [ Html.h2 []
            [ img
                [ src "images/icon-180.png"
                , width 50
                ]
                []
            , text " Elmlog"
            ]
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
                    , style "height" "10em"
                    , value model.userText
                    ]
                    [ text model.userText ]
                , fieldset
                    [ style "border-block-color" "gray"
                    , style "border-style" "dashed"
                    , style "border-color" "lightgray"
                    , style "border-width" "1"
                    ]
                    [ legend [] [ text "Input format" ]
                    , radioButton
                        { buttonValue = MarkdownInput
                        , radioValue = model.inputType
                        , radioName = inputTypeRadioName
                        , setter = SetInputType
                        , label = "Markdown"
                        }
                    , br
                    , radioButton
                        { buttonValue = FilteredHtmlInput
                        , radioValue = model.inputType
                        , radioName = inputTypeRadioName
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
                        , radioName = inputTypeRadioName
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
                        , radioName = inputTypeRadioName
                        , setter = SetInputType
                        , label = "Raw HTML"
                        }
                    ]
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
                    setter radioValue
            ]
            []
        , span
            [ onClick <| setter radioValue
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
            { model
                | userText = string
                , preview = toHtml string model.inputType
            }
                |> withNoCmd

        SetInputType inputType ->
            ( Debug.log
                ("SetInputType \"" ++ Debug.toString inputType ++ "\", model")
                { model
                    | inputType = inputType
                    , preview = toHtml model.userText inputType
                }
            , Cmd.none
            )

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


toHtml : String -> InputType -> Html Msg
toHtml string inputType =
    case inputType of
        FilteredHtmlInput ->
            parseHtml string FilteredHtmlInput

        FullHtmlInput ->
            parseHtml string FullHtmlInput

        RawHtmlInput ->
            parseHtml string RawHtmlInput

        MarkdownInput ->
            Markdown.toHtml [] string


parseHtml : String -> InputType -> Html Msg
parseHtml string messageType =
    -- TODO
    text <| Debug.log "parseHtml" string
