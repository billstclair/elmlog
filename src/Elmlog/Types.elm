---------------------------------------------------------------------
--
-- Types.elm
-- Shared types for Elmlog.
-- Copyright (c) 2026 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Elmlog.Types exposing (Message(..), MessageType(..), messageText)

{-| Elmlog types.
-}


{-| User message text.
-}
type Message
    = TextMessage String
    | FilteredHtmlMessage String
    | PlainHtmlMessage String
    | RawHtmlMessage String
    | MarkdownMessage String


messageText : Message -> String
messageText message =
    case message of
        TextMessage string ->
            string

        FilteredHtmlMessage string ->
            string

        PlainHtmlMessage string ->
            string

        RawHtmlMessage string ->
            string

        MarkdownMessage string ->
            string


{-| Message type.
-}
type MessageType
    = FilteredType
    | PlainType
    | RawType
    | MarkdownType
