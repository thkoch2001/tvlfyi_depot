module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Piano
import Random
import Random.List
import Tempo
import Theory
import Time exposing (..)
import UI


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
    , view : View
    }


type View
    = Preferences
    | Practice


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
    | ToggleInversion Theory.ChordInversion
    | ToggleChordType Theory.ChordType
    | TogglePitchClass Theory.PitchClass
    | ToggleKey Theory.Key
    | DoNothing
    | SetPracticeMode PracticeMode
    | SelectAllKeys
    | DeselectAllKeys
    | SetView View


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
            ( Theory.C3, Theory.C6 )

        inversions =
            Theory.allInversions

        chordTypes =
            Theory.allChordTypes

        pitchClasses =
            Theory.allPitchClasses

        keys =
            [ { pitchClass = Theory.C, mode = Theory.MajorMode } ]

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
    , tempo = 20
    , firstNote = firstNote
    , lastNote = lastNote
    , view = Preferences
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

        SetView x ->
            ( { model
                | view = x
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


selectKey :
    Model
    ->
        { relativeMajor : Theory.Key
        , relativeMinor : Theory.Key
        }
    -> Html Msg
selectKey model { relativeMajor, relativeMinor } =
    let
        active key =
            List.member key model.whitelistedKeys

        buttonLabel major minor =
            Theory.viewKey major ++ ", " ++ Theory.viewKey minor
    in
    div [ class "flex pt-0" ]
        [ UI.textToggleButton
            { label = buttonLabel relativeMajor relativeMinor
            , handleClick = ToggleKey relativeMinor
            , classes = [ "flex-1" ]
            , toggled = active relativeMinor || active relativeMajor
            }
        ]


keyCheckboxes : Model -> Html Msg
keyCheckboxes model =
    let
        majorKey pitchClass =
            { pitchClass = pitchClass, mode = Theory.MajorMode }

        minorKey pitchClass =
            { pitchClass = pitchClass, mode = Theory.MinorMode }

        circleOfFifths =
            [ ( Theory.C, Theory.A )
            , ( Theory.G, Theory.E )
            , ( Theory.D, Theory.B )
            , ( Theory.A, Theory.F_sharp )
            , ( Theory.E, Theory.C_sharp )
            , ( Theory.B, Theory.G_sharp )
            , ( Theory.F_sharp, Theory.D_sharp )
            , ( Theory.C_sharp, Theory.A_sharp )
            , ( Theory.G_sharp, Theory.F )
            , ( Theory.D_sharp, Theory.C )
            , ( Theory.A_sharp, Theory.G )
            , ( Theory.F, Theory.D )
            ]
    in
    div []
        [ h2 [ class "text-gray-500 text-center pt-10 text-5xl" ] [ text "Select keys" ]
        , ul []
            (circleOfFifths
                |> List.map
                    (\( major, minor ) ->
                        selectKey model
                            { relativeMajor = majorKey major
                            , relativeMinor = minorKey minor
                            }
                    )
            )
        ]


practiceModeButtons : Model -> Html Msg
practiceModeButtons model =
    div [ class "text-center" ]
        [ h2 [ class "py-10 text-5xl" ] [ text "Practice Mode" ]
        , div [ class "flex pb-6" ]
            [ UI.simpleButton
                { label = "Key"
                , classes = [ "flex-1", "rounded-r-none" ]
                , handleClick = SetPracticeMode KeyMode
                , color =
                    if model.practiceMode == KeyMode then
                        UI.Primary

                    else
                        UI.Secondary
                }
            , UI.simpleButton
                { label = "Fine Tune"
                , handleClick = SetPracticeMode FineTuneMode
                , classes = [ "flex-1", "rounded-l-none" ]
                , color =
                    if model.practiceMode == FineTuneMode then
                        UI.Primary

                    else
                        UI.Secondary
                }
            ]
        ]


openPreferences : Html Msg
openPreferences =
    button
        [ class "w-48 h-48 absolute left-0 top-0 z-40"
        , onClick (SetView Preferences)
        ]
        [ Icon.cog ]


closePreferences : Html Msg
closePreferences =
    button
        [ class "w-48 h-48 absolute right-0 top-0 z-10"
        , onClick (SetView Practice)
        ]
        [ Icon.close ]


preferences : Model -> Html Msg
preferences model =
    div [ class "pt-10 pb-20 px-10" ]
        [ closePreferences
        , Tempo.render
            { tempo = model.tempo
            , handleInput = SetTempo
            }
        , case model.practiceMode of
            KeyMode ->
                keyCheckboxes model

            FineTuneMode ->
                div []
                    [ inversionCheckboxes model.whitelistedInversions
                    , chordTypeCheckboxes model.whitelistedChordTypes
                    ]
        ]


practice : Model -> Html Msg
practice model =
    let
        ( handleClick, buttonText ) =
            if model.isPaused then
                ( Play, "Press to practice" )

            else
                ( Pause, "" )
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


view : Model -> Html Msg
view model =
    case model.view of
        Preferences ->
            preferences model

        Practice ->
            practice model


{-| For now, I'm just dumping things onto the page to sketch ideas.
-}
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
