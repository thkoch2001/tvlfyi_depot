module Practice exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Piano
import State
import Theory
import UI


openPreferences : Html State.Msg
openPreferences =
    button
        [ class "w-48 h-48 absolute left-0 top-0 z-40"
        , onClick (State.SetView State.Preferences)
        ]
        [ Icon.cog ]


render : State.Model -> Html State.Msg
render model =
    let
        ( handleClick, buttonText ) =
            if model.isPaused then
                ( State.Play, "Press to practice" )

            else
                ( State.Pause, "" )
    in
    div []
        [ openPreferences
        , UI.overlayButton
            { label = buttonText
            , handleClick = handleClick
            , isVisible = model.isPaused
            }
        , Piano.render
            { highlight = model.selectedChord |> Maybe.andThen Theory.notesForChord |> Maybe.withDefault []
            , start = model.firstNote
            , end = model.lastNote
            }
        ]
