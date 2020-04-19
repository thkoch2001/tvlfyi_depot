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
    | ToggleKey Theory.Key
    | DoNothing
    | SetView View
    | ToggleFlashCard


type View
    = Preferences
    | Practice
    | Overview


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
    , view : View
    , showFlashCard : Bool
    }


{-| The initial state for the application.
-}
init : Model
init =
    let
        ( firstNote, lastNote ) =
            ( Theory.C3, Theory.C6 )

        inversions =
            [ Theory.Root ]

        chordTypes =
            Theory.allChordTypes

        pitchClasses =
            Theory.allPitchClasses

        keys =
            [ { pitchClass = Theory.C, mode = Theory.MajorMode } ]
    in
    { whitelistedChords =
        keys
            |> List.concatMap Theory.chordsForKey
            |> List.filter (\chord -> List.member chord.chordInversion inversions)
    , whitelistedChordTypes = chordTypes
    , whitelistedInversions = inversions
    , whitelistedPitchClasses = pitchClasses
    , whitelistedKeys = keys
    , selectedChord = Nothing
    , isPaused = True
    , tempo = 10
    , firstNote = firstNote
    , lastNote = lastNote
    , view = Preferences
    , showFlashCard = True
    }


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

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
                    model.whitelistedKeys
                        |> List.concatMap Theory.chordsForKey
                        |> List.filter (\chord -> List.member chord.chordInversion inversions)
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
                    keys
                        |> List.concatMap Theory.chordsForKey
                        |> List.filter (\chord -> List.member chord.chordInversion model.whitelistedInversions)
                , selectedChord = Nothing
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

        ToggleFlashCard ->
            ( { model | showFlashCard = not model.showFlashCard }, Cmd.none )
