module Theory exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
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
type PitchClass
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
    { pitchClass : PitchClass
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


type alias NoteMetadata =
    { note : Note
    , label : String
    , pitchClass : PitchClass
    , natural : Bool
    }


{-| An integer representing which note in a given scale to play.
-}
type alias ScaleDegree =
    Int


{-| Returns the Note in the cental octave of the piano for a given
PitchClass. For example, C4 -- or "middle C" -- for C.
-}
noteInCentralOctave : PitchClass -> Note
noteInCentralOctave pitchClass =
    case pitchClass of
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
            List.map up [ Whole, Whole, Half, Whole, Whole, Whole ]

        MinorMode ->
            List.map up [ Whole, Half, Whole, Whole, Half, Whole ]

        BluesMode ->
            List.map up [ MinorThird, Whole, Half, Half, MinorThird ]


{-| Return a list of the intervals that a chord. Each interval measures
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
                |> (\x -> walkNotes x note)
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
walkNotes : List IntervalVector -> Note -> Maybe (List Note)
walkNotes steps note =
    doWalkNotes steps note [] |> Maybe.map List.reverse


{-| Recursive helper for `walkNotes`.
-}
doWalkNotes : List IntervalVector -> Note -> List Note -> Maybe (List Note)
doWalkNotes steps note result =
    case steps of
        [] ->
            Just (note :: result)

        s :: rest ->
            case step s note of
                Just x ->
                    doWalkNotes rest x (note :: result)

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


{-| Return the PitchClass for a given note.
-}
classifyNote : Note -> PitchClass
classifyNote note =
    note |> getNoteMetadata |> .pitchClass


{-| Return a list of the notes that comprise a `chord`
-}
notesForChord : Chord -> Maybe (List Note)
notesForChord { note, chordType, chordInversion } =
    intervalsForChordType chordType chordInversion
        |> List.map (\interval -> step interval note)
        |> Maybe.Extra.combine
        |> Maybe.map (\notes -> note :: notes)


{-| Return the scale for a given `key`.
-}
notesForKey : Key -> List Note
notesForKey { pitchClass, mode } =
    let
        origin =
            noteInCentralOctave pitchClass
    in
    case walkNotes (intervalsForMode mode) origin of
        -- We should never hit the Nothing case here.
        Nothing ->
            []

        Just scale ->
            scale


{-| Return true if `note` is a black key.
-}
isAccidental : Note -> Bool
isAccidental note =
    note |> isNatural |> not


{-| Return true if `note` is a white key.
-}
isNatural : Note -> Bool
isNatural note =
    note |> getNoteMetadata |> .natural


{-| Return a list of all of the notes that we know about.
Only return the notes within the range `start` and `end`.
-}
notesFromRange : Note -> Note -> List Note
notesFromRange start end =
    noteMetadata
        |> Array.toList
        |> List.map .note
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


{-| Return a list of all of the key modes about which we know.
-}
allModes : List Mode
allModes =
    [ MajorMode, MinorMode, BluesMode ]


{-| Return a list of all of the keys about which we know.
-}
allKeys : List Key
allKeys =
    allPitchClasses
        |> List.Extra.andThen
            (\pitchClass ->
                allModes
                    |> List.Extra.andThen
                        (\mode ->
                            [ { pitchClass = pitchClass
                              , mode = mode
                              }
                            ]
                        )
            )


{-| Return an array of every note on a piano.
Note: Currently this piano has 85 keys, but modern pianos have 88 keys. I would
prefer to have 88 keys, but it's not urgent.
-}
noteMetadata : Array NoteMetadata
noteMetadata =
    Array.fromList
        [ { note = A1, label = "A1", pitchClass = A, natural = True }
        , { note = A_sharp1, label = "A♯/B♭1", pitchClass = A_sharp, natural = False }
        , { note = B1, label = "B1", pitchClass = B, natural = True }
        , { note = C1, label = "C1", pitchClass = C, natural = True }
        , { note = C_sharp1, label = "C♯/D♭1", pitchClass = C_sharp, natural = False }
        , { note = D1, label = "D1", pitchClass = D, natural = True }
        , { note = D_sharp1, label = "D♯/E♭1", pitchClass = D_sharp, natural = False }
        , { note = E1, label = "E1", pitchClass = E, natural = True }
        , { note = F1, label = "F1", pitchClass = F, natural = True }
        , { note = F_sharp1, label = "F♯/G♭1", pitchClass = F_sharp, natural = False }
        , { note = G1, label = "G1", pitchClass = G, natural = True }
        , { note = G_sharp1, label = "G♯/A♭1", pitchClass = G, natural = False }
        , { note = A2, label = "A2", pitchClass = A, natural = True }
        , { note = A_sharp2, label = "A♯/B♭2", pitchClass = A_sharp, natural = False }
        , { note = B2, label = "B2", pitchClass = B, natural = True }
        , { note = C2, label = "C2", pitchClass = C, natural = True }
        , { note = C_sharp2, label = "C♯/D♭2", pitchClass = C_sharp, natural = False }
        , { note = D2, label = "D2", pitchClass = D, natural = True }
        , { note = D_sharp2, label = "D♯/E♭2", pitchClass = D_sharp, natural = False }
        , { note = E2, label = "E2", pitchClass = E, natural = True }
        , { note = F2, label = "F2", pitchClass = F, natural = True }
        , { note = F_sharp2, label = "F♯/G♭2", pitchClass = F_sharp, natural = False }
        , { note = G2, label = "G2", pitchClass = G, natural = True }
        , { note = G_sharp2, label = "G♯/A♭2", pitchClass = G, natural = False }
        , { note = A3, label = "A3", pitchClass = A, natural = True }
        , { note = A_sharp3, label = "A♯/B♭3", pitchClass = A_sharp, natural = False }
        , { note = B3, label = "B3", pitchClass = B, natural = True }
        , { note = C3, label = "C3", pitchClass = C, natural = True }
        , { note = C_sharp3, label = "C♯/D♭3", pitchClass = C_sharp, natural = False }
        , { note = D3, label = "D3", pitchClass = D, natural = True }
        , { note = D_sharp3, label = "D♯/E♭3", pitchClass = D_sharp, natural = False }
        , { note = E3, label = "E3", pitchClass = E, natural = True }
        , { note = F3, label = "F3", pitchClass = F, natural = True }
        , { note = F_sharp3, label = "F♯/G♭3", pitchClass = F_sharp, natural = False }
        , { note = G3, label = "G3", pitchClass = G, natural = True }
        , { note = G_sharp3, label = "G♯/A♭3", pitchClass = G, natural = False }
        , { note = A4, label = "A4", pitchClass = A, natural = True }
        , { note = A_sharp4, label = "A♯/B♭4", pitchClass = A_sharp, natural = False }
        , { note = B4, label = "B4", pitchClass = B, natural = True }
        , { note = C4, label = "C4", pitchClass = C, natural = True }
        , { note = C_sharp4, label = "C♯/D♭4", pitchClass = C_sharp, natural = False }
        , { note = D4, label = "D4", pitchClass = D, natural = True }
        , { note = D_sharp4, label = "D♯/E♭4", pitchClass = D_sharp, natural = False }
        , { note = E4, label = "E4", pitchClass = E, natural = True }
        , { note = F4, label = "F4", pitchClass = F, natural = True }
        , { note = F_sharp4, label = "F♯/G♭4", pitchClass = F_sharp, natural = False }
        , { note = G4, label = "G4", pitchClass = G, natural = True }
        , { note = G_sharp4, label = "G♯/A♭4", pitchClass = G, natural = False }
        , { note = A5, label = "A5", pitchClass = A, natural = True }
        , { note = A_sharp5, label = "A♯/B♭5", pitchClass = A_sharp, natural = False }
        , { note = B5, label = "B5", pitchClass = B, natural = True }
        , { note = C5, label = "C5", pitchClass = C, natural = True }
        , { note = C_sharp5, label = "C♯/D♭5", pitchClass = C_sharp, natural = False }
        , { note = D5, label = "D5", pitchClass = D, natural = True }
        , { note = D_sharp5, label = "D♯/E♭5", pitchClass = D_sharp, natural = False }
        , { note = E5, label = "E5", pitchClass = E, natural = True }
        , { note = F5, label = "F5", pitchClass = F, natural = True }
        , { note = F_sharp5, label = "F♯/G♭5", pitchClass = F_sharp, natural = False }
        , { note = G5, label = "G5", pitchClass = G, natural = True }
        , { note = G_sharp5, label = "G♯/A♭5", pitchClass = G, natural = False }
        , { note = A6, label = "A6", pitchClass = A, natural = True }
        , { note = A_sharp6, label = "A♯/B♭6", pitchClass = A_sharp, natural = False }
        , { note = B6, label = "B6", pitchClass = B, natural = True }
        , { note = C6, label = "C6", pitchClass = C, natural = True }
        , { note = C_sharp6, label = "C♯/D♭6", pitchClass = C_sharp, natural = False }
        , { note = D6, label = "D6", pitchClass = D, natural = True }
        , { note = D_sharp6, label = "D♯/E♭6", pitchClass = D_sharp, natural = False }
        , { note = E6, label = "E6", pitchClass = E, natural = True }
        , { note = F6, label = "F6", pitchClass = F, natural = True }
        , { note = F_sharp6, label = "F♯/G♭6", pitchClass = F_sharp, natural = False }
        , { note = G6, label = "G6", pitchClass = G, natural = True }
        , { note = G_sharp6, label = "G♯/A♭6", pitchClass = G, natural = False }
        , { note = A7, label = "A7", pitchClass = A, natural = True }
        , { note = A_sharp7, label = "A♯/B♭7", pitchClass = A_sharp, natural = False }
        , { note = B7, label = "B7", pitchClass = B, natural = True }
        , { note = C7, label = "C7", pitchClass = C, natural = True }
        , { note = C_sharp7, label = "C♯/D♭7", pitchClass = C_sharp, natural = False }
        , { note = D7, label = "D7", pitchClass = D, natural = True }
        , { note = D_sharp7, label = "D♯/E♭7", pitchClass = D_sharp, natural = False }
        , { note = E7, label = "E7", pitchClass = E, natural = True }
        , { note = F7, label = "F7", pitchClass = F, natural = True }
        , { note = F_sharp7, label = "F♯/G♭7", pitchClass = F_sharp, natural = False }
        , { note = G7, label = "G7", pitchClass = G, natural = True }
        , { note = G_sharp7, label = "G♯/A♭7", pitchClass = G, natural = False }
        , { note = C8, label = "C8", pitchClass = C, natural = True }
        ]


{-| Mapping of note data to commonly needed metadata for that note.
-}
getNoteMetadata : Note -> NoteMetadata
getNoteMetadata note =
    case Array.get (noteAsNumber note) noteMetadata of
        Just metadata ->
            metadata

        -- This case should never hit, so we just return C1 to appease the
        -- compiler.
        Nothing ->
            getNoteMetadata C1


{-| Return the numeric representation of `note` to ues when comparing two
notes.
-}
noteAsNumber : Note -> Int
noteAsNumber note =
    let
        result =
            noteMetadata
                |> Array.toList
                |> List.indexedMap Tuple.pair
                |> Misc.find (\( _, x ) -> x.note == note)
    in
    case result of
        Nothing ->
            0

        Just ( i, _ ) ->
            i


{-| Return true if all of the notes that comprise `chord` can be played on a
piano whose keys begin at `start` and end at `end`.
-}
chordWithinRange : Note -> Note -> Chord -> Bool
chordWithinRange start end chord =
    case notesForChord chord of
        Just notes ->
            let
                nums =
                    List.map noteAsNumber notes

                lo =
                    List.minimum nums |> Maybe.withDefault (noteAsNumber start)

                hi =
                    List.maximum nums |> Maybe.withDefault (noteAsNumber end)
            in
            lo >= noteAsNumber start && hi < noteAsNumber end

        Nothing ->
            False


{-| Return a list of all of the pitch classes that we know about.
-}
allPitchClasses : List PitchClass
allPitchClasses =
    [ C
    , C_sharp
    , D
    , D_sharp
    , E
    , F
    , F_sharp
    , G
    , G_sharp
    , A
    , A_sharp
    , B
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
    , pitchClasses : List PitchClass
    }
    -> List Chord
allChords { start, end, inversions, chordTypes, pitchClasses } =
    let
        notes =
            notesFromRange start end
                |> List.filter (\note -> List.member (classifyNote note) pitchClasses)
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
        |> List.filter (chordWithinRange start end)


{-| Return a human-readable format of `note`.
-}
viewNote : Note -> String
viewNote note =
    note |> getNoteMetadata |> .label


{-| Return a human-readable format of `chord`.
-}
viewChord : Chord -> String
viewChord { note, chordType, chordInversion } =
    viewPitchClass (classifyNote note) ++ " " ++ chordTypeName chordType ++ " " ++ inversionName chordInversion ++ " position"


{-| Return a human-readable format of `pitchClass`.
-}
viewPitchClass : PitchClass -> String
viewPitchClass pitchClass =
    case pitchClass of
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


viewMode : Mode -> String
viewMode mode =
    case mode of
        MajorMode ->
            "major"

        MinorMode ->
            "minor"

        BluesMode ->
            "blues"


{-| Return the human-readable format of `key`.
-}
viewKey : Key -> String
viewKey { pitchClass, mode } =
    viewPitchClass pitchClass ++ " " ++ viewMode mode


{-| Returns a pairing of a scale-degree to the type of chord at that scale
degree.
-}
practiceChordsForMode : Mode -> Dict ScaleDegree ChordType
practiceChordsForMode mode =
    case mode of
        MajorMode ->
            Dict.fromList
                [ ( 1, Major )
                , ( 2, Minor )
                , ( 3, Minor )
                , ( 4, Major )
                , ( 5, Major )
                , ( 6, Minor )
                , ( 7, Diminished )
                ]

        MinorMode ->
            Dict.fromList
                [ ( 1, Minor )
                , ( 2, Diminished )
                , ( 3, Major )
                , ( 4, Minor )
                , ( 5, Minor )
                , ( 6, Major )
                , ( 7, Major )
                ]

        BluesMode ->
            Dict.fromList
                [ ( 1, MajorDominant7 )

                -- While many refer to the blues progression as a I-IV-V, the IV
                -- chord is really a MajorDominant7 made from the third scale
                -- degree.
                , ( 3, MajorDominant7 )
                , ( 5, MajorDominant7 )
                ]


{-| Returns a list of chords for a particular `key`.
-}
chordsForKey : Key -> List Chord
chordsForKey key =
    let
        chords =
            practiceChordsForMode key.mode
    in
    notesForKey key
        |> List.indexedMap
            (\i note ->
                case Dict.get (i + 1) chords of
                    Nothing ->
                        Nothing

                    Just chordType ->
                        Just
                            (allInversions
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
        |> Maybe.Extra.values
        |> List.concat
