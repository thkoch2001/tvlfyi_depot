module Theory exposing (..)

import List.Extra
import Maybe.Extra
import Misc


{-| Notes are the individuals sounds that we use to create music. Think: "do re
mi fa so la ti do".

Note: Technically a "C-sharp" is also a "D-flat", but I will model accidentals
(i.e. sharps and flats) as sharps and represent the ambiguity when I render the
underlying state of the application.

Note: There are "notes" like A, B, D-flat, and then there are notes like "middle
C", also denoted in scientific pitch notation as C4. I'm unsure of what to call
each of these, and my application does not model scientific pitch notation yet,
so these non-scientific pitch denote values are "notes" for now.

-}
type Note
    = C1
    | C_sharp1
    | D1
    | D_sharp1
    | E1
    | F1
    | F_sharp1
    | G1
    | G_sharp1
    | A1
    | A_sharp1
    | B1
    | C2
    | C_sharp2
    | D2
    | D_sharp2
    | E2
    | F2
    | F_sharp2
    | G2
    | G_sharp2
    | A2
    | A_sharp2
    | B2
    | C3
    | C_sharp3
    | D3
    | D_sharp3
    | E3
    | F3
    | F_sharp3
    | G3
    | G_sharp3
    | A3
    | A_sharp3
    | B3
    | C4
    | C_sharp4
    | D4
    | D_sharp4
    | E4
    | F4
    | F_sharp4
    | G4
    | G_sharp4
    | A4
    | A_sharp4
    | B4
    | C5
    | C_sharp5
    | D5
    | D_sharp5
    | E5
    | F5
    | F_sharp5
    | G5
    | G_sharp5
    | A5
    | A_sharp5
    | B5
    | C6
    | C_sharp6
    | D6
    | D_sharp6
    | E6
    | F6
    | F_sharp6
    | G6
    | G_sharp6
    | A6
    | A_sharp6
    | B6
    | C7
    | C_sharp7
    | D7
    | D_sharp7
    | E7
    | F7
    | F_sharp7
    | G7
    | G_sharp7
    | A7
    | A_sharp7
    | B7
    | C8


{-| I alluded to this concept in the Note type's documentation. These are the
letters of notes. For instance C2, C3, C4 are all instances of C.
-}
type NoteClass
    = C
    | C_sharp
    | D
    | D_sharp
    | E
    | F
    | F_sharp
    | G
    | G_sharp
    | A
    | A_sharp
    | B


{-| Encode whether you are traversing "up" or "down" intervals
-}
type StepDirection
    = Up
    | Down


{-| One can measure the difference between between notes using intervals.
-}
type Interval
    = Half
    | NHalves Int
    | Whole
    | MajorThird
    | MinorThird
    | PerfectFifth
    | AugmentedFifth
    | DiminishedFifth
    | MajorSeventh
    | DominantSeventh


{-| Add direction to a distance on the piano.
-}
type alias IntervalVector =
    { interval : Interval
    , direction : StepDirection
    }


{-| A bundle of notes which are usually, but not necessarily harmonious.
-}
type alias Chord =
    { note : Note
    , chordType : ChordType
    , chordInversion : ChordInversion
    }


{-| Many possible chords exist. This type encodes the possibilities. I am
tempted to model these in a more "DRY" way, but I worry that this abstraction
may cause more problems than it solves.
-}
type ChordType
    = Major
    | Sus2
    | Sus4
    | Major7
    | MajorDominant7
    | Minor
    | MinorMajor7
    | MinorDominant7
    | Augmented
    | AugmentedDominant7
    | Diminished
    | DiminishedDominant7
    | DiminishedMajor7


{-| On a piano, a triad can be played three ways. As a rule-of-thumb, The number
of ways a pianist can play a chord is equal to the number of notes in the chord
itself.
-}
type ChordInversion
    = Root
    | First
    | Second


{-| Whether a given note is a white key or a black key.
-}
type KeyClass
    = Natural
    | Accidental


{-| Songs are written in one or more keys, which define the notes and therefore
chords that harmonize with one another.
-}
type alias Key =
    { noteClass : NoteClass
    , mode : Mode
    }


{-| We create "scales" by enumerating the notes of a given key. These keys are
defined by the "tonic" note and the "mode". I thought about including Ionian,
Dorian, Phrygian, etc., but in the I would like to avoid over-abstracting this
early on, so I'm going to err on the side of overly concrete until I have a
better idea of the extent of this project.
-}
type Mode
    = BluesMode
    | MajorMode
    | MinorMode


scaleDegree : Int -> Key -> NoteClass
scaleDegree which { noteClass } =
    case noteClass of
        _ ->
            C


{-| Returns the Note in the cental octave of the piano for a given
NoteClass. For example, C4 -- or "middle C" -- for C.
-}
noteInCentralOctave : NoteClass -> Note
noteInCentralOctave noteClass =
    case noteClass of
        C ->
            C4

        C_sharp ->
            C_sharp4

        D ->
            D4

        D_sharp ->
            D_sharp4

        E ->
            E4

        F ->
            F4

        F_sharp ->
            F_sharp4

        G ->
            G4

        G_sharp ->
            G_sharp4

        A ->
            A4

        A_sharp ->
            A_sharp4

        B ->
            B4


{-| Return the human-readable version of a chord inversion.
-}
inversionName : ChordInversion -> String
inversionName inversion =
    case inversion of
        Root ->
            "Root"

        First ->
            "First"

        Second ->
            "Second"


{-| Return the human-readable version of a chord type.
-}
chordTypeName : ChordType -> String
chordTypeName chordType =
    case chordType of
        Major ->
            "major"

        Sus2 ->
            "suspended 2"

        Sus4 ->
            "suspended 4"

        Major7 ->
            "major 7th"

        MajorDominant7 ->
            "major dominant 7th"

        Minor ->
            "minor"

        MinorMajor7 ->
            "minor major 7th"

        MinorDominant7 ->
            "minor dominant 7th"

        Augmented ->
            "augmented"

        AugmentedDominant7 ->
            "augmented dominant 7th"

        Diminished ->
            "diminished"

        DiminishedDominant7 ->
            "diminished dominant 7th"

        DiminishedMajor7 ->
            "diminished major 7th"


{-| Return the note that is one half step away from `note` in the direction,
`dir`.
In the case of stepping up or down from the end of the piano, this returns a
Maybe.
-}
halfStep : StepDirection -> Note -> Maybe Note
halfStep dir note =
    let
        everyNote =
            notesFromRange C2 C8
    in
    case dir of
        Up ->
            Misc.comesAfter note everyNote

        Down ->
            Misc.comesBefore note everyNote


{-| Return a list of steps to take away from the root note to return back to the
root note for a given mode.
-}
intervalsForMode : Mode -> List IntervalVector
intervalsForMode mode =
    let
        up x =
            { direction = Up, interval = x }

        down x =
            { direction = Down, interval = x }
    in
    case mode of
        MajorMode ->
            List.map up [ Whole, Whole, Half, Whole, Whole, Whole, Half ]

        MinorMode ->
            List.map up [ Whole, Half, Whole, Whole, Half, Whole, Whole ]

        BluesMode ->
            List.map up [ MinorThird, Whole, Half, Half, MinorThird ]


{-| Return a list of the intervals the comprise a chord. Each interval measures
the distance away from the root-note of the chord.
-}
intervalsForChordType : ChordType -> ChordInversion -> List IntervalVector
intervalsForChordType chordType chordInversion =
    let
        up x =
            { direction = Up, interval = x }

        down x =
            { direction = Down, interval = x }
    in
    case ( chordType, chordInversion ) of
        -- Major
        ( Major, Root ) ->
            [ up MajorThird, up PerfectFifth ]

        ( Major, First ) ->
            [ down (NHalves 5), down (NHalves 8) ]

        ( Major, Second ) ->
            [ down (NHalves 5), up MajorThird ]

        -- Sus2
        ( Sus2, Root ) ->
            [ up Whole, up PerfectFifth ]

        ( Sus2, First ) ->
            [ down (NHalves 10), down (NHalves 5) ]

        ( Sus2, Second ) ->
            [ down (NHalves 5), up Whole ]

        -- Sus4
        ( Sus4, Root ) ->
            [ up (NHalves 5), up PerfectFifth ]

        ( Sus4, First ) ->
            [ down (NHalves 7), down (NHalves 5) ]

        ( Sus4, Second ) ->
            [ down (NHalves 5), up (NHalves 5) ]

        -- Major7
        ( Major7, Root ) ->
            [ up MajorThird, up PerfectFifth, up MajorSeventh ]

        ( Major7, First ) ->
            down Half :: intervalsForChordType Major chordInversion

        ( Major7, Second ) ->
            down Half :: intervalsForChordType Major chordInversion

        -- MajorDominant7
        ( MajorDominant7, Root ) ->
            up DominantSeventh :: intervalsForChordType Major chordInversion

        ( MajorDominant7, First ) ->
            down Whole :: intervalsForChordType Major chordInversion

        ( MajorDominant7, Second ) ->
            down Whole :: intervalsForChordType Major chordInversion

        -- Minor
        ( Minor, Root ) ->
            [ up MinorThird, up PerfectFifth ]

        ( Minor, First ) ->
            [ down (NHalves 5), down (NHalves 9) ]

        ( Minor, Second ) ->
            [ down (NHalves 5), up MinorThird ]

        -- MinorMajor7
        ( MinorMajor7, Root ) ->
            up MajorSeventh :: intervalsForChordType Minor chordInversion

        ( MinorMajor7, First ) ->
            down Half :: intervalsForChordType Minor chordInversion

        ( MinorMajor7, Second ) ->
            down Half :: intervalsForChordType Minor chordInversion

        -- MinorDominant7
        ( MinorDominant7, Root ) ->
            up DominantSeventh :: intervalsForChordType Minor chordInversion

        ( MinorDominant7, First ) ->
            down Whole :: intervalsForChordType Minor chordInversion

        ( MinorDominant7, Second ) ->
            down Whole :: intervalsForChordType Minor chordInversion

        -- Augmented
        ( Augmented, Root ) ->
            [ up MajorThird, up AugmentedFifth ]

        ( Augmented, First ) ->
            [ down (NHalves 8), down (NHalves 4) ]

        ( Augmented, Second ) ->
            [ down (NHalves 4), up MajorThird ]

        -- AugmentedDominant7
        ( AugmentedDominant7, Root ) ->
            up DominantSeventh :: intervalsForChordType Augmented chordInversion

        ( AugmentedDominant7, First ) ->
            down Whole :: intervalsForChordType Augmented chordInversion

        ( AugmentedDominant7, Second ) ->
            down Whole :: intervalsForChordType Augmented chordInversion

        -- Diminished
        ( Diminished, Root ) ->
            [ up MinorThird, up DiminishedFifth ]

        ( Diminished, First ) ->
            [ down (NHalves 6), down (NHalves 9) ]

        ( Diminished, Second ) ->
            [ down (NHalves 6), up MinorThird ]

        -- DiminishedDominant7
        ( DiminishedDominant7, Root ) ->
            up DominantSeventh :: intervalsForChordType Diminished chordInversion

        ( DiminishedDominant7, First ) ->
            down Whole :: intervalsForChordType Diminished chordInversion

        ( DiminishedDominant7, Second ) ->
            down Whole :: intervalsForChordType Diminished chordInversion

        -- DiminishedMajor7
        ( DiminishedMajor7, Root ) ->
            up MajorSeventh :: intervalsForChordType Diminished chordInversion

        ( DiminishedMajor7, First ) ->
            down Half :: intervalsForChordType Diminished chordInversion

        ( DiminishedMajor7, Second ) ->
            down Half :: intervalsForChordType Diminished chordInversion


{-| Return the note in the direction, `dir`, away from `note` `s` intervals
-}
step : IntervalVector -> Note -> Maybe Note
step { direction, interval } note =
    let
        doStep int =
            step { direction = direction, interval = int }
    in
    case interval of
        Half ->
            halfStep direction note

        NHalves n ->
            List.repeat n
                { direction = direction
                , interval = Half
                }
                |> (\x -> applySteps x note)
                |> Maybe.andThen (List.reverse >> List.head)

        Whole ->
            note
                |> doStep Half
                |> Maybe.andThen (doStep Half)

        MinorThird ->
            note
                |> doStep Whole
                |> Maybe.andThen (doStep Half)

        MajorThird ->
            note
                |> doStep Whole
                |> Maybe.andThen (doStep Whole)

        PerfectFifth ->
            note
                |> doStep MajorThird
                |> Maybe.andThen (doStep MinorThird)

        AugmentedFifth ->
            note
                |> doStep PerfectFifth
                |> Maybe.andThen (doStep Half)

        DiminishedFifth ->
            note
                |> doStep MajorThird
                |> Maybe.andThen (doStep Whole)

        MajorSeventh ->
            note
                |> doStep PerfectFifth
                |> Maybe.andThen (doStep MajorThird)

        DominantSeventh ->
            note
                |> doStep PerfectFifth
                |> Maybe.andThen (doStep MinorThird)


{-| Returns a list of all of the notes away from a give `note`.

  - The 0th element is applied to `note`.
  - The 1st element is applied to the result of the previous operation.
  - The 2nd element is applied to the result of the previous operation.
  - and so on...until all of the `steps` are exhausted.

In the case where applying any of the steps would result in running off of
either edge of the piano, this function returns a Nothing.

-}
applySteps : List IntervalVector -> Note -> Maybe (List Note)
applySteps steps note =
    doApplySteps steps note [] |> Maybe.map List.reverse


doApplySteps : List IntervalVector -> Note -> List Note -> Maybe (List Note)
doApplySteps steps note result =
    case steps of
        [] ->
            Just (note :: result)

        s :: rest ->
            case step s note of
                Just x ->
                    doApplySteps rest x (note :: result)

                Nothing ->
                    Nothing


{-| Return the KeyClass for a given `note`.
-}
keyClass : Note -> KeyClass
keyClass note =
    if isNatural note then
        Natural

    else
        Accidental


{-| Return the NoteClass for a given note.
-}
classifyNote : Note -> NoteClass
classifyNote note =
    if List.member note [ C1, C2, C3, C4, C5, C6, C7, C8 ] then
        C

    else if List.member note [ C_sharp1, C_sharp2, C_sharp3, C_sharp4, C_sharp5, C_sharp6, C_sharp7 ] then
        C_sharp

    else if List.member note [ D1, D2, D3, D4, D5, D6, D7 ] then
        D

    else if List.member note [ D_sharp1, D_sharp2, D_sharp3, D_sharp4, D_sharp5, D_sharp6, D_sharp7 ] then
        D_sharp

    else if List.member note [ E1, E2, E3, E4, E5, E6, E7 ] then
        E

    else if List.member note [ F1, F2, F3, F4, F5, F6, F7 ] then
        F

    else if List.member note [ F_sharp1, F_sharp2, F_sharp3, F_sharp4, F_sharp5, F_sharp6, F_sharp7 ] then
        F_sharp

    else if List.member note [ G1, G2, G3, G4, G5, G6, G7 ] then
        G

    else if List.member note [ G_sharp1, G_sharp2, G_sharp3, G_sharp4, G_sharp5, G_sharp6, G_sharp7 ] then
        G_sharp

    else if List.member note [ A1, A2, A3, A4, A5, A6, A7 ] then
        A

    else if List.member note [ A_sharp1, A_sharp2, A_sharp3, A_sharp4, A_sharp5, A_sharp6, A_sharp7 ] then
        A_sharp

    else
        B


{-| Return a list of the notes that comprise a `chord`
-}
notesForChord : Chord -> Maybe (List Note)
notesForChord { note, chordType, chordInversion } =
    intervalsForChordType chordType chordInversion
        |> List.map (\interval -> step interval note)
        |> Maybe.Extra.combine
        |> Maybe.map (\notes -> note :: notes)


{-| Return the scale for a given `key`
-}
notesForKey : Key -> List Note
notesForKey { noteClass, mode } =
    let
        origin =
            noteInCentralOctave noteClass
    in
    case applySteps (intervalsForMode mode) origin of
        -- We should never hit the Nothing case here.
        Nothing ->
            []

        Just scale ->
            scale


{-| Return true if `note` is a black key.
-}
isAccidental : Note -> Bool
isAccidental note =
    case classifyNote note of
        C ->
            False

        C_sharp ->
            True

        D ->
            False

        D_sharp ->
            True

        E ->
            False

        F ->
            False

        F_sharp ->
            True

        G ->
            False

        G_sharp ->
            True

        A ->
            False

        A_sharp ->
            True

        B ->
            False


{-| Return true if `note` is a white key.
-}
isNatural : Note -> Bool
isNatural note =
    note |> isAccidental |> not


{-| Return a list of all of the notes that we know about.
Only return the notes within the range `start` and `end`.
-}
notesFromRange : Note -> Note -> List Note
notesFromRange start end =
    [ C1
    , C_sharp1
    , D1
    , D_sharp1
    , E1
    , F1
    , F_sharp1
    , G1
    , G_sharp1
    , A1
    , A_sharp1
    , B1
    , C2
    , C_sharp2
    , D2
    , D_sharp2
    , E2
    , F2
    , F_sharp2
    , G2
    , G_sharp2
    , A2
    , A_sharp2
    , B2
    , C3
    , C_sharp3
    , D3
    , D_sharp3
    , E3
    , F3
    , F_sharp3
    , G3
    , G_sharp3
    , A3
    , A_sharp3
    , B3
    , C4
    , C_sharp4
    , D4
    , D_sharp4
    , E4
    , F4
    , F_sharp4
    , G4
    , G_sharp4
    , A4
    , A_sharp4
    , B4
    , C5
    , C_sharp5
    , D5
    , D_sharp5
    , E5
    , F5
    , F_sharp5
    , G5
    , G_sharp5
    , A5
    , A_sharp5
    , B5
    , C6
    , C_sharp6
    , D6
    , D_sharp6
    , E6
    , F6
    , F_sharp6
    , G6
    , G_sharp6
    , A6
    , A_sharp6
    , B6
    , C7
    , C_sharp7
    , D7
    , D_sharp7
    , E7
    , F7
    , F_sharp7
    , G7
    , G_sharp7
    , A7
    , A_sharp7
    , B7
    , C8
    ]
        |> List.Extra.dropWhile ((/=) start)
        |> List.Extra.takeWhile ((/=) end)


{-| Return a list of all of the chord inversions about which we know.
-}
allInversions : List ChordInversion
allInversions =
    [ Root, First, Second ]


{-| Return a list of all of the chord types about which we know.
-}
allChordTypes : List ChordType
allChordTypes =
    [ Major
    , Sus2
    , Sus4
    , Major7
    , MajorDominant7
    , Minor
    , MinorMajor7
    , MinorDominant7
    , Augmented
    , AugmentedDominant7
    , Diminished
    , DiminishedDominant7
    , DiminishedMajor7
    ]


{-| Return a list of all of the chords that we know about.
Only create chords from the range of notes delimited by the range `start` and
`end`.
-}
allChords :
    { start : Note
    , end : Note
    , inversions : List ChordInversion
    , chordTypes : List ChordType
    }
    -> List Chord
allChords { start, end, inversions, chordTypes } =
    let
        notes =
            notesFromRange start end
    in
    notes
        |> List.Extra.andThen
            (\note ->
                chordTypes
                    |> List.Extra.andThen
                        (\chordType ->
                            inversions
                                |> List.Extra.andThen
                                    (\inversion ->
                                        [ { note = note
                                          , chordType = chordType
                                          , chordInversion = inversion
                                          }
                                        ]
                                    )
                        )
            )


{-| Serialize a human-readable format of `note`.
-}
viewNote : Note -> String
viewNote note =
    case note of
        C1 ->
            "C1"

        C_sharp1 ->
            "C♯/D♭1"

        D1 ->
            "D1"

        D_sharp1 ->
            "D♯/E♭1"

        E1 ->
            "E1"

        F1 ->
            "F1"

        F_sharp1 ->
            "F♯/G♭1"

        G1 ->
            "G1"

        G_sharp1 ->
            "G♯/A♭1"

        A1 ->
            "A1"

        A_sharp1 ->
            "A♯/B♭1"

        B1 ->
            "B1"

        C2 ->
            "C2"

        C_sharp2 ->
            "C♯/D♭2"

        D2 ->
            "D2"

        D_sharp2 ->
            "D♯/E♭2"

        E2 ->
            "E2"

        F2 ->
            "F2"

        F_sharp2 ->
            "F♯/G♭2"

        G2 ->
            "G2"

        G_sharp2 ->
            "G♯/A♭2"

        A2 ->
            "A2"

        A_sharp2 ->
            "A♯/B♭2"

        B2 ->
            "B2"

        C3 ->
            "C3"

        C_sharp3 ->
            "C♯/D♭3"

        D3 ->
            "D3"

        D_sharp3 ->
            "D♯/E♭3"

        E3 ->
            "E3"

        F3 ->
            "F3"

        F_sharp3 ->
            "F♯/G♭3"

        G3 ->
            "G3"

        G_sharp3 ->
            "G♯/A♭3"

        A3 ->
            "A3"

        A_sharp3 ->
            "A♯/B♭3"

        B3 ->
            "B3"

        C4 ->
            "C4"

        C_sharp4 ->
            "C♯/D♭4"

        D4 ->
            "D4"

        D_sharp4 ->
            "D♯/E♭4"

        E4 ->
            "E4"

        F4 ->
            "F4"

        F_sharp4 ->
            "F♯/G♭4"

        G4 ->
            "G4"

        G_sharp4 ->
            "G♯/A♭4"

        A4 ->
            "A4"

        A_sharp4 ->
            "A♯/B♭4"

        B4 ->
            "B4"

        C5 ->
            "C5"

        C_sharp5 ->
            "C♯/D♭5"

        D5 ->
            "D5"

        D_sharp5 ->
            "D♯/E♭5"

        E5 ->
            "E5"

        F5 ->
            "F5"

        F_sharp5 ->
            "F♯/G♭5"

        G5 ->
            "G5"

        G_sharp5 ->
            "G♯/A♭5"

        A5 ->
            "A5"

        A_sharp5 ->
            "A♯/B♭5"

        B5 ->
            "B5"

        C6 ->
            "C6"

        C_sharp6 ->
            "C♯/D♭6"

        D6 ->
            "D6"

        D_sharp6 ->
            "D♯/E♭6"

        E6 ->
            "E6"

        F6 ->
            "F6"

        F_sharp6 ->
            "F♯/G♭6"

        G6 ->
            "G6"

        G_sharp6 ->
            "G♯/A♭6"

        A6 ->
            "A6"

        A_sharp6 ->
            "A♯/B♭6"

        B6 ->
            "B6"

        C7 ->
            "C7"

        C_sharp7 ->
            "C♯/D♭7"

        D7 ->
            "D7"

        D_sharp7 ->
            "D♯/E♭7"

        E7 ->
            "E7"

        F7 ->
            "F7"

        F_sharp7 ->
            "F♯/G♭7"

        G7 ->
            "G7"

        G_sharp7 ->
            "G♯/A♭7"

        A7 ->
            "A7"

        A_sharp7 ->
            "A♯/B♭7"

        B7 ->
            "B7"

        C8 ->
            "C8"


inspectChord : Chord -> String
inspectChord { note, chordType, chordInversion } =
    viewNote note ++ " " ++ chordTypeName chordType ++ " " ++ inversionName chordInversion ++ " position"


viewChord : Chord -> String
viewChord { note, chordType, chordInversion } =
    viewNoteClass (classifyNote note) ++ " " ++ chordTypeName chordType ++ " " ++ inversionName chordInversion ++ " position"


{-| Serialize a human-readable format of `noteClass`.
-}
viewNoteClass : NoteClass -> String
viewNoteClass noteClass =
    case noteClass of
        C ->
            "C"

        C_sharp ->
            "C♯/D♭"

        D ->
            "D"

        D_sharp ->
            "D♯/E♭"

        E ->
            "E"

        F ->
            "F"

        F_sharp ->
            "F♯/G♭"

        G ->
            "G"

        G_sharp ->
            "G♯/A♭"

        A ->
            "A"

        A_sharp ->
            "A♯/B♭"

        B ->
            "B"
