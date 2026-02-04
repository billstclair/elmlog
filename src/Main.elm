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
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
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
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { url = url
    , key = key
    }
        |> withNoCmd


h2 : String -> Html msg
h2 string =
    Html.h2 [] [ text string ]


view : Model -> Document msg
view model =
    { title = "Elmlog"
    , body =
        [ h2 "Elmlog"
        , a [ href "https://github.com/billstclair/elmlog" ]
            [ text "GitHub" ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model |> withNoCmd
