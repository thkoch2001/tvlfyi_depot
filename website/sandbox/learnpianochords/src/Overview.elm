module Overview exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State


render : State.Model -> Html State.Msg
render model =
    div [] [ text "Hello, Overview" ]
