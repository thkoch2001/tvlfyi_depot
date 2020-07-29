module Login exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State

googleSignIn : Html State.Msg
googleSignIn =
    div [ class "g-signin2", attribute "onsuccess" "onSignIn" ] []

render : State.Model -> Html State.Msg
render model =
    div [ class "pt-10 pb-20 px-10" ]
        [ googleSignIn
        ]
