module Main exposing (main)

import Browser
import Html exposing (..)
import Misc
import Overview
import Practice
import Preferences
import State
import Time exposing (..)


subscriptions : State.Model -> Sub State.Msg
subscriptions model =
    if model.isPaused then
        Sub.none

    else
        Time.every (model.tempo |> Misc.bpmToMilliseconds |> toFloat) (\_ -> State.NextChord)


view : State.Model -> Html State.Msg
view model =
    case model.view of
        State.Preferences ->
            Preferences.render model

        State.Practice ->
            Practice.render model

        State.Overview ->
            Overview.render model


main =
    Browser.element
        { init = \() -> ( State.init, Cmd.none )
        , subscriptions = subscriptions
        , update = State.update
        , view = view
        }
