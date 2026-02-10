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


module Elmlog.Types exposing (InputType(..))

{-| User input type.
-}


type InputType
    = MarkdownInput
    | FilteredHtmlInput
    | FullHtmlInput
    | RawHtmlInput
