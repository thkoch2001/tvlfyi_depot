module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


button : List (Attribute msg) -> List (Html msg) -> Html msg
button attrs children =
    Html.button ([ class "focus:outline-none" ] ++ attrs) children
