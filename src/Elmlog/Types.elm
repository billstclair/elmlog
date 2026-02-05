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


module Elmlog.Types exposing (Message(..), MessageType(..))

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


{-| Message type.
-}
type MessageType
    = Filtered
    | Plain
    | Raw
