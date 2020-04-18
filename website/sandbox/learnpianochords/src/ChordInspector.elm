module ChordInspector exposing (render)

import Html exposing (..)
import NoteInspector
import Theory


render : Theory.Chord -> Html a
render chord =
    case Theory.notesForChord chord of
        Nothing ->
            p [] [ text "Cannot retrieve the notes for the chord." ]

        Just notes ->
            NoteInspector.render notes
