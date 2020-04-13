module Main exposing (main)

import Browser
import ChordInspector
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NoteInspector
import Piano
import Random
import Random.List
import Tempo
import Theory
import Time exposing (..)


type alias Model =
    { whitelistedChords : List Theory.Chord
    , whitelistedChordTypes : List Theory.ChordType
    , whitelistedInversions : List Theory.ChordInversion
    , whitelistedPitchClasses : List Theory.PitchClass
    , whitelistedKeys : List Theory.Key
    , selectedChord : Maybe Theory.Chord
    , isPaused : Bool
    , tempo : Int
    , firstNote : Theory.Note
    , lastNote : Theory.Note
    , practiceMode : PracticeMode
    , debug :
        { enable : Bool
        , inspectChord : Bool
        }
    }


{-| Control the type of practice you'd like.
-}
type PracticeMode
    = KeyMode
    | FineTuneMode


type Msg
    = NextChord
    | NewChord Theory.Chord
    | Play
    | Pause
    | IncreaseTempo
    | DecreaseTempo
    | SetTempo String
    | ToggleInspectChord
    | ToggleInversion Theory.ChordInversion
    | ToggleChordType Theory.ChordType
    | TogglePitchClass Theory.PitchClass
    | ToggleKey Theory.Key
    | DoNothing
    | SetPracticeMode PracticeMode
    | SelectAllKeys
    | DeselectAllKeys


{-| The amount by which we increase or decrease tempo.
-}
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


{-| The initial state for the application.
-}
init : Model
init =
    let
        ( firstNote, lastNote ) =
            ( Theory.C3, Theory.C5 )

        inversions =
            Theory.allInversions

        chordTypes =
            Theory.allChordTypes

        pitchClasses =
            Theory.allPitchClasses

        keys =
            Theory.allKeys

        practiceMode =
            KeyMode
    in
    { practiceMode = practiceMode
    , whitelistedChords =
        case practiceMode of
            KeyMode ->
                keys |> List.concatMap Theory.chordsForKey

            FineTuneMode ->
                Theory.allChords
                    { start = firstNote
                    , end = lastNote
                    , inversions = inversions
                    , chordTypes = chordTypes
                    , pitchClasses = pitchClasses
                    }
    , whitelistedChordTypes = chordTypes
    , whitelistedInversions = inversions
    , whitelistedPitchClasses = pitchClasses
    , whitelistedKeys = keys
    , selectedChord = Nothing
    , isPaused = True
    , tempo = 60
    , firstNote = firstNote
    , lastNote = lastNote
    , debug =
        { enable = False
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
        DoNothing ->
            ( model, Cmd.none )

        SetPracticeMode practiceMode ->
            ( { model
                | practiceMode = practiceMode
                , isPaused = True
              }
            , Cmd.none
            )

        SelectAllKeys ->
            ( { model
                | whitelistedKeys = Theory.allKeys
                , whitelistedChords =
                    Theory.allKeys |> List.concatMap Theory.chordsForKey
              }
            , Cmd.none
            )

        DeselectAllKeys ->
            ( { model
                | whitelistedKeys = []
                , whitelistedChords = []
              }
            , Cmd.none
            )

        NewChord chord ->
            ( { model | selectedChord = Just chord }
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
                            DoNothing
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

        ToggleChordType chordType ->
            let
                chordTypes =
                    if List.member chordType model.whitelistedChordTypes then
                        List.filter ((/=) chordType) model.whitelistedChordTypes

                    else
                        chordType :: model.whitelistedChordTypes
            in
            ( { model
                | whitelistedChordTypes = chordTypes
                , whitelistedChords =
                    Theory.allChords
                        { start = model.firstNote
                        , end = model.lastNote
                        , inversions = model.whitelistedInversions
                        , chordTypes = chordTypes
                        , pitchClasses = model.whitelistedPitchClasses
                        }
              }
            , Cmd.none
            )

        ToggleInversion inversion ->
            let
                inversions =
                    if List.member inversion model.whitelistedInversions then
                        List.filter ((/=) inversion) model.whitelistedInversions

                    else
                        inversion :: model.whitelistedInversions
            in
            ( { model
                | whitelistedInversions = inversions
                , whitelistedChords =
                    Theory.allChords
                        { start = model.firstNote
                        , end = model.lastNote
                        , inversions = inversions
                        , chordTypes = model.whitelistedChordTypes
                        , pitchClasses = model.whitelistedPitchClasses
                        }
              }
            , Cmd.none
            )

        TogglePitchClass pitchClass ->
            let
                pitchClasses =
                    if List.member pitchClass model.whitelistedPitchClasses then
                        List.filter ((/=) pitchClass) model.whitelistedPitchClasses

                    else
                        pitchClass :: model.whitelistedPitchClasses
            in
            ( { model
                | whitelistedPitchClasses = pitchClasses
                , whitelistedChords =
                    Theory.allChords
                        { start = model.firstNote
                        , end = model.lastNote
                        , inversions = model.whitelistedInversions
                        , chordTypes = model.whitelistedChordTypes
                        , pitchClasses = pitchClasses
                        }
              }
            , Cmd.none
            )

        ToggleKey key ->
            let
                keys =
                    if List.member key model.whitelistedKeys then
                        List.filter ((/=) key) model.whitelistedKeys

                    else
                        key :: model.whitelistedKeys
            in
            ( { model
                | whitelistedKeys = keys
                , whitelistedChords =
                    keys |> List.concatMap Theory.chordsForKey
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


pitchClassCheckboxes : List Theory.PitchClass -> Html Msg
pitchClassCheckboxes pitchClasses =
    ul []
        (Theory.allPitchClasses
            |> List.map
                (\pitchClass ->
                    li []
                        [ label [] [ text (Theory.viewPitchClass pitchClass) ]
                        , input
                            [ type_ "checkbox"
                            , onClick (TogglePitchClass pitchClass)
                            , checked (List.member pitchClass pitchClasses)
                            ]
                            []
                        ]
                )
        )


chordTypeCheckboxes : List Theory.ChordType -> Html Msg
chordTypeCheckboxes chordTypes =
    ul []
        (Theory.allChordTypes
            |> List.map
                (\chordType ->
                    li []
                        [ label [] [ text (Theory.chordTypeName chordType) ]
                        , input
                            [ type_ "checkbox"
                            , onClick (ToggleChordType chordType)
                            , checked (List.member chordType chordTypes)
                            ]
                            []
                        ]
                )
        )


inversionCheckboxes : List Theory.ChordInversion -> Html Msg
inversionCheckboxes inversions =
    ul []
        (Theory.allInversions
            |> List.map
                (\inversion ->
                    li []
                        [ label [] [ text (Theory.inversionName inversion) ]
                        , input
                            [ type_ "checkbox"
                            , onClick (ToggleInversion inversion)
                            , checked (List.member inversion inversions)
                            ]
                            []
                        ]
                )
        )


keyCheckboxes : List Theory.Key -> Html Msg
keyCheckboxes keys =
    div []
        [ h2 [] [ text "Choose Key" ]
        , button [ onClick SelectAllKeys ] [ text "Select all" ]
        , button [ onClick DeselectAllKeys ] [ text "Deselect all" ]
        , ul []
            (Theory.allKeys
                |> List.map
                    (\key ->
                        li []
                            [ label [] [ text (Theory.viewKey key) ]
                            , input
                                [ type_ "checkbox"
                                , onClick (ToggleKey key)
                                , checked (List.member key keys)
                                ]
                                []
                            ]
                    )
            )
        ]


displayChord :
    { debug : Bool
    , chord : Theory.Chord
    , firstNote : Theory.Note
    , lastNote : Theory.Note
    }
    -> Html Msg
displayChord { debug, chord, firstNote, lastNote } =
    div []
        [ if debug then
            ChordInspector.render chord

          else
            span [] []
        , p [] [ text (Theory.viewChord chord) ]
        , case Theory.notesForChord chord of
            Just x ->
                Piano.render
                    { highlight = x
                    , start = firstNote
                    , end = lastNote
                    }

            Nothing ->
                p [] [ text "No chord to show" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ Tempo.render
            { tempo = model.tempo
            , handleIncrease = IncreaseTempo
            , handleDecrease = DecreaseTempo
            , handleInput = SetTempo
            }
        , div []
            [ h2 [] [ text "Practice Mode" ]
            , input
                [ type_ "radio"
                , id "key-mode"
                , name "key-mode"
                , checked (model.practiceMode == KeyMode)
                , onClick (SetPracticeMode KeyMode)
                ]
                []
            , label [ for "key-mode" ] [ text "Key Mode" ]
            , input
                [ type_ "radio"
                , id "fine-tune-mode"
                , name "fine-tune-mode"
                , checked (model.practiceMode == FineTuneMode)
                , onClick (SetPracticeMode FineTuneMode)
                ]
                []
            , label [ for "fine-tune-mode" ] [ text "Fine-tuning Mode" ]
            ]
        , case model.practiceMode of
            KeyMode ->
                keyCheckboxes model.whitelistedKeys

            FineTuneMode ->
                div []
                    [ pitchClassCheckboxes model.whitelistedPitchClasses
                    , inversionCheckboxes model.whitelistedInversions
                    , chordTypeCheckboxes model.whitelistedChordTypes
                    ]
        , playPause model
        , if model.debug.enable then
            debugger

          else
            span [] []
        , case model.selectedChord of
            Just chord ->
                displayChord
                    { debug = model.debug.inspectChord
                    , chord = chord
                    , firstNote = model.firstNote
                    , lastNote = model.lastNote
                    }

            Nothing ->
                p [] [ text "No chord to display" ]
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
