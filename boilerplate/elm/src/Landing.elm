module Landing exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State


render : State.Model -> Html State.Msg
render model =
    div [ class "pt-10 pb-20 px-10" ]
        [ p [] [ text "Welcome to the landing page!" ]
        ]
