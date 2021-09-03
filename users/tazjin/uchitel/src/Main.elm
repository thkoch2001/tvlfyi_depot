module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = (), update = \() m -> m, view = \() -> text "Hello World" }
