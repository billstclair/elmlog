#|- API.elm
  - How to make posts.
  -|#

module Elmlog.Api exposing (post, read)

#|- Filename
-|
type alias Filename String

#|- post to a filename
-|#
post : Filename -> String -> String
post filename string =
    string

#- read
-|#
read : Filename -> String
read filename =
    "String at " ++ filename
