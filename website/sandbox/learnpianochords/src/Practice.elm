module Practice exposing (render)

import FlashCard
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Piano
import State
import Tailwind
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
        , case model.selectedChord of
            Just chord ->
                FlashCard.render
                    { chord = chord
                    , visible = model.showFlashCard
                    }

            Nothing ->
                -- Here I'm abusing the overlayButton component to render text
                -- horizontally. I should support a UI component for this.
                UI.overlayButton
                    { label = "Get ready..."
                    , handleClick = State.DoNothing
                    , isVisible = True
                    }
        , UI.overlayButton
            { label = buttonText
            , handleClick = handleClick
            , isVisible = model.isPaused
            }
        , Piano.render
            { chord = model.selectedChord
            , firstNote = model.firstNote
            , lastNote = model.lastNote
            }
        ]
