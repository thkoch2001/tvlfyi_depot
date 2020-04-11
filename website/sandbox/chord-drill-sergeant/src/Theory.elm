module Theory exposing (..)

import List.Extra
import Misc

{-| Notes are the individuals sounds that we use to create music. Think: "do re
mi fa so la ti do".

Note: Technically a "C-sharp" is also a "D-flat", but I will model accidentals
(i.e. sharps and flats) as sharps and represent the ambiguity when I render the
underlying state of the application.

Note: There are "notes" like A, B, D-flat, and then there are notes like "middle
C", also denoted in scientific pitch notation as C4. I'm unsure of what to call
each of these, and my application does not model scientific pitch notation yet,
so these non-scientific pitch denote values are "notes" for now. -}
type Note = C1 | C_sharp1 | D1 | D_sharp1 | E1 | F1 | F_sharp1 | G1 | G_sharp1 | A1 | A_sharp1 | B1
          | C2 | C_sharp2 | D2 | D_sharp2 | E2 | F2 | F_sharp2 | G2 | G_sharp2 | A2 | A_sharp2 | B2
          | C3 | C_sharp3 | D3 | D_sharp3 | E3 | F3 | F_sharp3 | G3 | G_sharp3 | A3 | A_sharp3 | B3
          | C4 | C_sharp4 | D4 | D_sharp4 | E4 | F4 | F_sharp4 | G4 | G_sharp4 | A4 | A_sharp4 | B4
          | C5 | C_sharp5 | D5 | D_sharp5 | E5 | F5 | F_sharp5 | G5 | G_sharp5 | A5 | A_sharp5 | B5
          | C6 | C_sharp6 | D6 | D_sharp6 | E6 | F6 | F_sharp6 | G6 | G_sharp6 | A6 | A_sharp6 | B6
          | C7 | C_sharp7 | D7 | D_sharp7 | E7 | F7 | F_sharp7 | G7 | G_sharp7 | A7 | A_sharp7 | B7
          | C8

{-| I alluded to this concept in the Note type's documentation. These are the
letters of notes. For instance C2, C3, C4 are all instances of C. -}
type NoteClass = C
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

{-| Encode whether you are traversing "up" or "down" intervals -}
type StepDirection = Up | Down

{-| One can measure the difference between between notes using intervals. -}
type Interval = Half
              | Whole
              | MajorThird
              | MinorThird

{-| A bundle of notes which are usually, but not necessarily harmonious. -}
type alias Chord =
  { note : Note
  , chordType : ChordType
  , chordPosition : ChordPosition
  }

{-| Many possible chords exist. This type encodes the possibilities. I am
tempted to model these in a more "DRY" way, but I worry that this abstraction
may cause more problems than it solves. -}
type ChordType = Major
               | Major7
               | MajorDominant7
               | Minor
               | Minor7
               | MinorDominant7
               | Augmented
               | Augmented7
               | Diminished
               | Diminished7

{-| On a piano, a triad can be played three ways. As a rule-of-thumb, The number
of ways a pianist can play a chord is equal to the number of notes in the chord
itself. -}
type ChordPosition = First
                   | Second
                   | Third
                   | Fourth

{-| Songs are written in one or more keys, which define the notes and therefore
chords that harmonize with one another. -}
type alias Key =
  { noteClass : NoteClass
  , mode : Mode
  }

{-| We create "scales" by enumerating the notes of a given key. These keys are
defined by the "tonic" note and the "mode".  I thought about including Ionian,
Dorian, Phrygian, etc., but in the I would like to avoid over-abstracting this
early on, so I'm going to err on the side of overly concrete until I have a
better idea of the extent of this project. -}
type Mode = BluesMode
          | MajorMode
          | MinorMode

{-| Returns the Note in the cental octave of the piano for a given
NoteClass. For example, C4 -- or "middle C" -- for C. -}
noteInCentralOctave : NoteClass -> Note
noteInCentralOctave noteClass =
    case noteClass of
        C       -> C4
        C_sharp -> C_sharp4
        D       -> D4
        D_sharp -> D_sharp4
        E       -> E4
        F       -> F4
        F_sharp -> F_sharp4
        G       -> G4
        G_sharp -> G_sharp4
        A       -> A4
        A_sharp -> A_sharp4
        B       -> B4

{-| Return the note that is one half step away from `note` in the direction,
`dir`.
In the case of stepping up or down from the end of the piano, this returns a
Maybe.
-}
halfStep : StepDirection -> Note -> Maybe Note
halfStep dir note =
  case dir of
    Up   -> Misc.comesAfter note allNotes
    Down -> Misc.comesBefore note allNotes

{-| Return a list of steps to take away from the root note to return back to the
root note for a given mode.
-}
intervalsForMode : Mode -> List Interval
intervalsForMode mode =
  case mode of
    MajorMode -> [Whole, Whole, Half, Whole, Whole, Whole, Half]
    MinorMode -> [Whole, Half, Whole, Whole, Half, Whole, Whole]
    BluesMode -> [MinorThird, Whole, Half, Half, MinorThird]

{-| Return a list of the intervals the comprise a chord -}
intervalsForChordType : ChordType -> List Interval
intervalsForChordType chordType =
  case chordType of
    Major          -> [MajorThird, MinorThird]
    Major7         -> [MajorThird, MinorThird, MajorThird]
    MajorDominant7 -> [MajorThird, MinorThird, MajorThird, MinorThird]
    Minor          -> [MinorThird, MajorThird]
    Minor7         -> [MinorThird, MajorThird, MajorThird]
    MinorDominant7 -> [MinorThird, MajorThird, MajorThird, MinorThird]
    Augmented      -> [MajorThird, MajorThird]
    Augmented7     -> [MajorThird, MajorThird, Whole]
    Diminished     -> [MinorThird, MinorThird]
    Diminished7    -> [MinorThird, MinorThird, MinorThird]

{-| Return the note in the direction, `dir`, away from `note` `s` intervals -}
step : StepDirection -> Interval -> Note -> Maybe Note
step dir s note =
  let
    doHalfStep = halfStep dir
  in
    case s of
      Half ->
          doHalfStep note
      Whole ->
          doHalfStep note
              |> Maybe.andThen doHalfStep
      MinorThird ->
          doHalfStep note
              |> Maybe.andThen doHalfStep
              |> Maybe.andThen doHalfStep
      MajorThird ->
          doHalfStep note
              |> Maybe.andThen doHalfStep
              |> Maybe.andThen doHalfStep
              |> Maybe.andThen doHalfStep

{-| Returns a list of all of the notes up from a give `note`.

In the case where applying all of the steps would result in running off of the
edge of the piano, this function returns a Maybe. -}
applySteps : List Interval -> Note -> Maybe (List Note)
applySteps steps note =
    doApplySteps steps note [] |> Maybe.map List.reverse

doApplySteps : List Interval -> Note -> List Note -> Maybe (List Note)
doApplySteps steps note result =
    case steps of
        [] -> Just (note::result)
        s::rest ->
            case step Up s note of
                Just x -> doApplySteps rest x (note::result)
                Nothing -> Nothing

{-| Return the NoteClass for a given note. -}
classifyNote : Note -> NoteClass
classifyNote note =
    if List.member note [C1, C2, C3, C4, C5, C6, C7, C8] then
        C
    else if List.member note [C_sharp1, C_sharp2, C_sharp3, C_sharp4, C_sharp5, C_sharp6, C_sharp7] then
        C_sharp
    else if List.member note [D1, D2, D3, D4, D5, D6, D7] then
        D
    else if List.member note [D_sharp1, D_sharp2, D_sharp3, D_sharp4, D_sharp5, D_sharp6, D_sharp7] then
        D_sharp
    else if List.member note [E1, E2, E3, E4, E5, E6, E7] then
        E
    else if List.member note [F1, F2, F3, F4, F5, F6, F7] then
        F
    else if List.member note [F_sharp1, F_sharp2, F_sharp3, F_sharp4, F_sharp5, F_sharp6, F_sharp7] then
        F_sharp
    else if List.member note [G1, G2, G3, G4, G5, G6, G7] then
        G
    else if List.member note [G_sharp1, G_sharp2, G_sharp3, G_sharp4, G_sharp5, G_sharp6, G_sharp7] then
        G_sharp
    else if List.member note [A1, A2, A3, A4, A5, A6, A7] then
        A
    else if List.member note [A_sharp1, A_sharp2, A_sharp3, A_sharp4, A_sharp5, A_sharp6, A_sharp7] then
        A_sharp
    else
        B

{-| Return a list of the notes that comprise a `chord` -}
notesForChord : Chord -> Maybe (List Note)
notesForChord {note, chordType} =
    case applySteps (intervalsForChordType chordType) note of
        Nothing -> Nothing
        Just notes -> Just <| note::notes

{-| Return the scale for a given `key` -}
notesForKey : Key -> List Note
notesForKey {noteClass, mode} =
    let origin = noteInCentralOctave noteClass
    in case applySteps (intervalsForMode mode) origin of
           -- We should never hit the Nothing case here.
           Nothing -> []
           Just scale -> scale

{-| Return a list of all of the notes that we know about. -}
allNotes : List Note
allNotes =
  [ C1 , C_sharp1 , D1 , D_sharp1 , E1 , F1 , F_sharp1 , G1 , G_sharp1 , A1 , A_sharp1 , B1
  , C2 , C_sharp2 , D2 , D_sharp2 , E2 , F2 , F_sharp2 , G2 , G_sharp2 , A2 , A_sharp2 , B2
  , C3 , C_sharp3 , D3 , D_sharp3 , E3 , F3 , F_sharp3 , G3 , G_sharp3 , A3 , A_sharp3 , B3
  , C4 , C_sharp4 , D4 , D_sharp4 , E4 , F4 , F_sharp4 , G4 , G_sharp4 , A4 , A_sharp4 , B4
  , C5 , C_sharp5 , D5 , D_sharp5 , E5 , F5 , F_sharp5 , G5 , G_sharp5 , A5 , A_sharp5 , B5
  , C6 , C_sharp6 , D6 , D_sharp6 , E6 , F6 , F_sharp6 , G6 , G_sharp6 , A6 , A_sharp6 , B6
  , C7 , C_sharp7 , D7 , D_sharp7 , E7 , F7 , F_sharp7 , G7 , G_sharp7 , A7 , A_sharp7 , B7
  , C8
  ]

{-| Return a list of all of the chords that we know about. -}
allChords : List Chord
allChords =
  let notes = allNotes
      chordTypes = [ Major
                   , Major7
                   , MajorDominant7
                   , Minor
                   , Minor7
                   , MinorDominant7
                   , Augmented
                   , Augmented7
                   , Diminished
                   , Diminished7
                   ]
      chordPositions = [ First
                       , Second
                       , Third
                       , Fourth
                       ] in
    notes
    |> List.Extra.andThen (\note -> chordTypes
    |> List.Extra.andThen (\chordType -> chordPositions
    |> List.Extra.andThen (\chordPosition -> [{ note = note
                                             , chordType = chordType
                                             , chordPosition = chordPosition
                                             }])))
