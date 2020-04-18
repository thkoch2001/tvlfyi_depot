module State exposing (..)

import Random
import Random.List
import Theory


type Msg
    = NextChord
    | NewChord Theory.Chord
    | Play
    | Pause
    | SetTempo String
    | ToggleInversion Theory.ChordInversion
    | ToggleChordType Theory.ChordType
    | TogglePitchClass Theory.PitchClass
    | ToggleKey Theory.Key
    | DoNothing
    | SetPracticeMode PracticeMode
    | SetView View


type View
    = Preferences
    | Practice
    | Overview


{-| Control the type of practice you'd like.
-}
type PracticeMode
    = KeyMode
    | FineTuneMode


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
    , view = Overview
    }


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
