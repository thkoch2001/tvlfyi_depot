module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.List

import Piano
import Theory

type State = State { whitelistedChords : List Theory.Chord
                   , selectedChord : Theory.Chord
                   }

type Msg = NextChord
         | NewChord Theory.Chord

viewChord : Theory.Chord -> String
viewChord (Theory.Chord (note, chordType, chordPosition)) =
  viewNote note ++ " " ++
  (case chordType of
     Theory.Major -> "major"
     Theory.Major7 -> "major 7th"
     Theory.MajorDominant7 -> "major dominant 7th"
     Theory.Minor -> "minor"
     Theory.Minor7 -> "minor 7th"
     Theory.MinorDominant7 -> "minor dominant 7th"
     Theory.Augmented -> "augmented"
     Theory.Augmented7 -> "augmented 7th"
     Theory.Diminished -> "diminished"
     Theory.Diminished7 -> "diminished 7th") ++ " " ++
  (case chordPosition of
     Theory.First -> "root position"
     Theory.Second -> "2nd position"
     Theory.Third -> "3rd position"
     Theory.Fourth -> "4th position")

{-| Serialize a human-readable format of `note` -}
viewNote : Theory.Note -> String
viewNote note =
  case note of
    Theory.C -> "C"
    Theory.C_sharp -> "C♯/D♭"
    Theory.D -> "D"
    Theory.D_sharp -> "D♯/E♭"
    Theory.E -> "E"
    Theory.F -> "F"
    Theory.F_sharp -> "F♯/G♭"
    Theory.G -> "G"
    Theory.G_sharp -> "G♯/A♭"
    Theory.A -> "A"
    Theory.A_sharp -> "A♯/B♭"
    Theory.B -> "B"

cmajor : Theory.Chord
cmajor = Theory.Chord (Theory.C, Theory.Major, Theory.First)

{-| The initial state for the application. -}
initialState : State
initialState =
  State { whitelistedChords = Theory.allChords
        , selectedChord = cmajor
        }

{-| Now that we have state, we need a function to change the state. -}
update : Msg -> State -> (State, Cmd Msg)
update msg (State {whitelistedChords, selectedChord}) =
  case msg of
    NewChord chord -> ( State { whitelistedChords = whitelistedChords
                              , selectedChord = chord
                              }
                      , Cmd.none
                      )
    NextChord -> ( State { whitelistedChords = whitelistedChords
                         , selectedChord = selectedChord
                         }
                 , Random.generate (\x ->
                                      case x of
                                        (Just chord, _) -> NewChord chord
                                        (Nothing, _)    -> NewChord cmajor)
                   (Random.List.choose whitelistedChords)
                 )

view : State -> Html Msg
view (State {selectedChord}) =
  div [] [ p [] [ text (viewChord selectedChord) ]
         , button [ onClick NextChord ] [ text "Next Chord" ]
         , Piano.render { highlight = Theory.notesForChord selectedChord }
         ]

{-| For now, I'm just dumping things onto the page to sketch ideas. -}
main =
  Browser.element { init = \() -> (initialState, Cmd.none)
                  , subscriptions = \_ -> Sub.none
                  , update = update
                  , view = view
                  }
