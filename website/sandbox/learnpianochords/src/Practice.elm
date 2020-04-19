module Practice exposing (render)

import FlashCard
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
        [ class "w-48 h-48 absolute left-0 top-0 z-50"
        , onClick (State.SetView State.Preferences)
        ]
        [ Icon.cog ]


render : State.Model -> Html State.Msg
render model =
    let
        ( handleClick, buttonText ) =
            if model.isPaused then
                ( State.Play, "Tap to practice" )

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
        , case model.selectedChord of
            Just chord ->
                FlashCard.render
                    { chord = chord
                    , visible = model.showFlashCard
                    }

            Nothing ->
                span [] []
        , Piano.render
            { chord = model.selectedChord
            , firstNote = model.firstNote
            , lastNote = model.lastNote
            }
        ]
