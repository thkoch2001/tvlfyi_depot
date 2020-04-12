module Main exposing (main)

import Browser
import ChordInspector
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Piano
import Random
import Random.List
import Tempo
import Theory
import Time exposing (..)


type alias Model =
    { whitelistedChords : List Theory.Chord
    , selectedChord : Theory.Chord
    , isPaused : Bool
    , tempo : Int
    , firstNote : Theory.Note
    , lastNote : Theory.Note
    , debug :
        { enable : Bool
        , inspectChord : Bool
        }
    }


type Msg
    = NextChord
    | NewChord Theory.Chord
    | Play
    | Pause
    | IncreaseTempo
    | DecreaseTempo
    | SetTempo String
    | ToggleInspectChord


tempoStep : Int
tempoStep =
    5


{-| Return the number of milliseconds that elapse during an interval in a
`target` bpm.
-}
bpmToMilliseconds : Int -> Int
bpmToMilliseconds target =
    let
        msPerMinute =
            1000 * 60
    in
    round (toFloat msPerMinute / toFloat target)


cmajor : Theory.Chord
cmajor =
    { note = Theory.C4
    , chordType = Theory.MajorDominant7
    , chordInversion = Theory.Root
    }


{-| The initial state for the application.
-}
init : Model
init =
    let
        ( firstNote, lastNote ) =
            ( Theory.C3, Theory.C5 )
    in
    { whitelistedChords = Theory.allChords firstNote lastNote
    , selectedChord = cmajor
    , isPaused = True
    , tempo = 60
    , firstNote = firstNote
    , lastNote = lastNote
    , debug =
        { enable = True
        , inspectChord = True
        }
    }


subscriptions : Model -> Sub Msg
subscriptions { isPaused, tempo } =
    if isPaused then
        Sub.none

    else
        Time.every (tempo |> bpmToMilliseconds |> toFloat) (\_ -> NextChord)


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewChord chord ->
            ( { model | selectedChord = chord }
            , Cmd.none
            )

        NextChord ->
            ( model
            , Random.generate
                (\x ->
                    case x of
                        ( Just chord, _ ) ->
                            NewChord chord

                        ( Nothing, _ ) ->
                            NewChord cmajor
                )
                (Random.List.choose model.whitelistedChords)
            )

        Play ->
            ( { model | isPaused = False }
            , Cmd.none
            )

        Pause ->
            ( { model | isPaused = True }
            , Cmd.none
            )

        IncreaseTempo ->
            ( { model | tempo = model.tempo + tempoStep }
            , Cmd.none
            )

        DecreaseTempo ->
            ( { model | tempo = model.tempo - tempoStep }
            , Cmd.none
            )

        ToggleInspectChord ->
            ( { model
                | debug =
                    { inspectChord = not model.debug.inspectChord
                    , enable = model.debug.enable
                    }
              }
            , Cmd.none
            )

        SetTempo tempo ->
            ( { model
                | tempo =
                    case String.toInt tempo of
                        Just x ->
                            x

                        Nothing ->
                            model.tempo
              }
            , Cmd.none
            )


playPause : Model -> Html Msg
playPause { isPaused } =
    if isPaused then
        button [ onClick Play ] [ text "Play" ]

    else
        button [ onClick Pause ] [ text "Pause" ]


debugger : Html Msg
debugger =
    fieldset []
        [ label [] [ text "Inspect Chord" ]
        , input [ type_ "checkbox", onClick ToggleInspectChord, checked init.debug.inspectChord ] []
        ]


view : Model -> Html Msg
view model =
    case Theory.notesForChord model.selectedChord of
        Nothing ->
            p [] [ text ("""
                       We cannot render the chord that you provided because the
                       notes that comprise the chord fall off either the upper
                       or lower end of the piano.

                       Chord:
                       """ ++ Theory.inspectChord model.selectedChord) ]

        Just x ->
            div []
                [ Tempo.render
                    { tempo = model.tempo
                    , handleIncrease = IncreaseTempo
                    , handleDecrease = DecreaseTempo
                    , handleInput = SetTempo
                    }
                , playPause model
                , if model.debug.enable then
                    debugger

                  else
                    span [] []
                , if model.debug.inspectChord then
                    ChordInspector.render model.selectedChord

                  else
                    span [] []
                , p [] [ text (Theory.viewChord model.selectedChord) ]
                , Piano.render
                    { highlight = x
                    , start = model.firstNote
                    , end = model.lastNote
                    }
                ]


{-| For now, I'm just dumping things onto the page to sketch ideas.
-}
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
