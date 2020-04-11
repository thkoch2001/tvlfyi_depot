module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.List
import Time exposing (..)

import Piano
import Theory
import Tempo

type alias Model =
  { whitelistedChords : List Theory.Chord
  , selectedChord : Theory.Chord
  , isPaused : Bool
  , tempo : Int
  }

type Msg = NextChord
         | NewChord Theory.Chord
         | Play
         | Pause
         | IncreaseTempo
         | DecreaseTempo
         | SetTempo String

tempoStep : Int
tempoStep = 5

{-| Return the number of milliseconds that elapse during an interval in a
`target` bpm.
-}
bpmToMilliseconds : Int -> Int
bpmToMilliseconds target =
  let msPerMinute = 1000 * 60
  in round (toFloat msPerMinute / toFloat target)

inspectChord : Theory.Chord -> String
inspectChord {note, chordType, chordPosition} =
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

viewChord : Theory.Chord -> String
viewChord {note, chordType, chordPosition} =
  viewNoteClass (Theory.classifyNote note) ++ " " ++
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

{-| Serialize a human-readable format of `note`. -}
viewNote : Theory.Note -> String
viewNote note =
  case note of
    Theory.C1       -> "C1"
    Theory.C_sharp1 -> "C♯/D♭1"
    Theory.D1       -> "D1"
    Theory.D_sharp1 -> "D♯/E♭1"
    Theory.E1       -> "E1"
    Theory.F1       -> "F1"
    Theory.F_sharp1 -> "F♯/G♭1"
    Theory.G1       -> "G1"
    Theory.G_sharp1 -> "G♯/A♭1"
    Theory.A1       -> "A1"
    Theory.A_sharp1 -> "A♯/B♭1"
    Theory.B1       -> "B1"
    Theory.C2       -> "C2"
    Theory.C_sharp2 -> "C♯/D♭2"
    Theory.D2       -> "D2"
    Theory.D_sharp2 -> "D♯/E♭2"
    Theory.E2       -> "E2"
    Theory.F2       -> "F2"
    Theory.F_sharp2 -> "F♯/G♭2"
    Theory.G2       -> "G2"
    Theory.G_sharp2 -> "G♯/A♭2"
    Theory.A2       -> "A2"
    Theory.A_sharp2 -> "A♯/B♭2"
    Theory.B2       -> "B2"
    Theory.C3       -> "C3"
    Theory.C_sharp3 -> "C♯/D♭3"
    Theory.D3       -> "D3"
    Theory.D_sharp3 -> "D♯/E♭3"
    Theory.E3       -> "E3"
    Theory.F3       -> "F3"
    Theory.F_sharp3 -> "F♯/G♭3"
    Theory.G3       -> "G3"
    Theory.G_sharp3 -> "G♯/A♭3"
    Theory.A3       -> "A3"
    Theory.A_sharp3 -> "A♯/B♭3"
    Theory.B3       -> "B3"
    Theory.C4       -> "C4"
    Theory.C_sharp4 -> "C♯/D♭4"
    Theory.D4       -> "D4"
    Theory.D_sharp4 -> "D♯/E♭4"
    Theory.E4       -> "E4"
    Theory.F4       -> "F4"
    Theory.F_sharp4 -> "F♯/G♭4"
    Theory.G4       -> "G4"
    Theory.G_sharp4 -> "G♯/A♭4"
    Theory.A4       -> "A4"
    Theory.A_sharp4 -> "A♯/B♭4"
    Theory.B4       -> "B4"
    Theory.C5       -> "C5"
    Theory.C_sharp5 -> "C♯/D♭5"
    Theory.D5       -> "D5"
    Theory.D_sharp5 -> "D♯/E♭5"
    Theory.E5       -> "E5"
    Theory.F5       -> "F5"
    Theory.F_sharp5 -> "F♯/G♭5"
    Theory.G5       -> "G5"
    Theory.G_sharp5 -> "G♯/A♭5"
    Theory.A5       -> "A5"
    Theory.A_sharp5 -> "A♯/B♭5"
    Theory.B5       -> "B5"
    Theory.C6       -> "C6"
    Theory.C_sharp6 -> "C♯/D♭6"
    Theory.D6       -> "D6"
    Theory.D_sharp6 -> "D♯/E♭6"
    Theory.E6       -> "E6"
    Theory.F6       -> "F6"
    Theory.F_sharp6 -> "F♯/G♭6"
    Theory.G6       -> "G6"
    Theory.G_sharp6 -> "G♯/A♭6"
    Theory.A6       -> "A6"
    Theory.A_sharp6 -> "A♯/B♭6"
    Theory.B6       -> "B6"
    Theory.C7       -> "C7"
    Theory.C_sharp7 -> "C♯/D♭7"
    Theory.D7       -> "D7"
    Theory.D_sharp7 -> "D♯/E♭7"
    Theory.E7       -> "E7"
    Theory.F7       -> "F7"
    Theory.F_sharp7 -> "F♯/G♭7"
    Theory.G7       -> "G7"
    Theory.G_sharp7 -> "G♯/A♭7"
    Theory.A7       -> "A7"
    Theory.A_sharp7 -> "A♯/B♭7"
    Theory.B7       -> "B7"
    Theory.C8       -> "C8"

{-| Serialize a human-readable format of `noteClass`. -}
viewNoteClass : Theory.NoteClass -> String
viewNoteClass noteClass =
  case noteClass of
    Theory.C       -> "C"
    Theory.C_sharp -> "C♯/D♭"
    Theory.D       -> "D"
    Theory.D_sharp -> "D♯/E♭"
    Theory.E       -> "E"
    Theory.F       -> "F"
    Theory.F_sharp -> "F♯/G♭"
    Theory.G       -> "G"
    Theory.G_sharp -> "G♯/A♭"
    Theory.A       -> "A"
    Theory.A_sharp -> "A♯/B♭"
    Theory.B       -> "B"

cmajor : Theory.Chord
cmajor =
  { note = Theory.C4
  , chordType = Theory.Major
  , chordPosition = Theory.First
  }

{-| The initial state for the application. -}
init : Model
init =
  { whitelistedChords = Theory.allChords
  , selectedChord = cmajor
  , isPaused = True
  , tempo = 60
  }

subscriptions : Model -> Sub Msg
subscriptions {isPaused, tempo} =
  if isPaused then
    Sub.none
  else
    Time.every (tempo |> bpmToMilliseconds |> toFloat) (\_ -> NextChord)

{-| Now that we have state, we need a function to change the state. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewChord chord -> ( { model | selectedChord = chord }
                      , Cmd.none
                      )
    NextChord -> ( model
                 , Random.generate (\x ->
                                      case x of
                                        (Just chord, _) -> NewChord chord
                                        (Nothing, _)    -> NewChord cmajor)
                   (Random.List.choose model.whitelistedChords)
                 )
    Play -> ( { model | isPaused = False }
            , Cmd.none
            )
    Pause -> ( { model | isPaused = True }
             , Cmd.none
             )
    IncreaseTempo -> ( { model | tempo = model.tempo + tempoStep }
                     , Cmd.none
                     )
    DecreaseTempo -> ( { model | tempo = model.tempo - tempoStep }
                     , Cmd.none
                     )
    SetTempo tempo -> ( { model |
                          tempo = case String.toInt tempo of
                                    Just x  -> x
                                    Nothing -> model.tempo
                         }
                      , Cmd.none
                      )

playPause : Model -> Html Msg
playPause {isPaused} =
  if isPaused then
    button [ onClick Play ] [ text "Play" ]
  else
    button [ onClick Pause ] [ text "Pause" ]

view : Model -> Html Msg
view model =
  case Theory.notesForChord model.selectedChord of
      Nothing ->
          p [] [ text ("""
                       We cannot render the chord that you provided because the
                       notes that comprise the chord fall off either the upper
                       or lower end of the piano.

                       Chord:
                       """ ++ (inspectChord model.selectedChord)) ]
      Just x ->
          div [] [ Tempo.render { tempo = model.tempo
                                , handleIncrease = IncreaseTempo
                                , handleDecrease = DecreaseTempo
                                , handleInput    = SetTempo
                                }
                 , playPause model
                 , p [] [ text (viewChord model.selectedChord) ]
                 , Piano.render { highlight = x }
                 ]

{-| For now, I'm just dumping things onto the page to sketch ideas. -}
main =
  Browser.element { init = \() -> (init, Cmd.none)
                  , subscriptions = subscriptions
                  , update = update
                  , view = view
                  }
