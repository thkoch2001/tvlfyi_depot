module Main exposing (main)

import Browser
import Html exposing (..)
import Landing
import Login
import State


subscriptions : State.Model -> Sub State.Msg
subscriptions model =
    Sub.none


view : State.Model -> Html State.Msg
view model =
    case model.view of
        State.Landing ->
            Landing.render model

        State.Login ->
            Login.render model


main =
    Browser.element
        { init = \() -> ( State.init, Cmd.none )
        , subscriptions = subscriptions
        , update = State.update
        , view = view
        }
