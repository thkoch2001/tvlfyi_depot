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

tempoStep : Int
tempoStep = 100

viewChord : Theory.Chord -> String
viewChord {note, chordType, chordPosition} =
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
cmajor =
  { note = Theory.C
  , chordType = Theory.Major
  , chordPosition = Theory.First
  }

{-| The initial state for the application. -}
init : Model
init =
  { whitelistedChords = Theory.allChords
  , selectedChord = cmajor
  , isPaused = True
  , tempo = 1000
  }

subscriptions : Model -> Sub Msg
subscriptions {isPaused, tempo} =
  if isPaused then
    Sub.none
  else
    Time.every (toFloat tempo) (\_ -> NextChord)

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
    IncreaseTempo -> ( { model | tempo = model.tempo - tempoStep }
                     , Cmd.none
                     )
    DecreaseTempo -> ( { model | tempo = model.tempo + tempoStep }
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
  div [] [ p [] [ text (viewChord model.selectedChord) ]
         , p [] [ text (String.fromInt model.tempo) ]
         , button [ onClick NextChord ] [ text "Next Chord" ]
         , button [ onClick IncreaseTempo ] [ text "Faster" ]
         , button [ onClick DecreaseTempo ] [ text "Slower" ]
         , playPause model
         , Piano.render { highlight = Theory.notesForChord model.selectedChord }
         ]

{-| For now, I'm just dumping things onto the page to sketch ideas. -}
main =
  Browser.element { init = \() -> (init, Cmd.none)
                  , subscriptions = subscriptions
                  , update = update
                  , view = view
                  }
