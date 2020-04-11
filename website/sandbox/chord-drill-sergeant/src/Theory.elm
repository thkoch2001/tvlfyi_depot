module Theory exposing (..)

import List.Extra

{-| Notes are the individuals sounds that we use to create music. Think: "do re
mi fa so la ti do".

Note: Technically a "C-sharp" is also a "D-flat", but I will model accidentals
(i.e. sharps and flats) as sharps and represent the ambiguity when I render the
underlying state of the application.

Note: There are "notes" like A, B, D-flat, and then there are notes like "middle
C", also denoted in scientific pitch notation as C4. I'm unsure of what to call
each of these, and my application does not model scientific pitch notation yet,
so these non-scientific pitch denote values are "notes" for now. -}
type Note = C
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
type Chord = Chord (Note, ChordType, ChordPosition)

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
type Key = Key (Note, Mode)

{-| We create "scales" by enumerating the notes of a given key. These keys are
defined by the "tonic" note and the "mode".  I thought about including Ionian,
Dorian, Phrygian, etc., but in the I would like to avoid over-abstracting this
early on, so I'm going to err on the side of overly concrete until I have a
better idea of the extent of this project. -}
type Mode = BluesMode
          | MajorMode
          | MinorMode

{-| Return the note that is one half step away from `note` in the direction,
`dir`.
-}
halfStep : StepDirection -> Note -> Note
halfStep dir note =
  case (dir, note) of
    -- C
    (Up, C) -> C_sharp
    (Down, C) -> B
    -- C#
    (Up, C_sharp) -> D
    (Down, C_sharp) -> C
    -- D
    (Up, D) -> D_sharp
    (Down, D) -> C_sharp
    -- D_sharp
    (Up, D_sharp) -> E
    (Down, D_sharp) -> D
    -- E
    (Up, E) -> F
    (Down, E) -> D_sharp
    -- F
    (Up, F) -> F_sharp
    (Down, F) -> E
    -- F#
    (Up, F_sharp) -> G
    (Down, F_sharp) -> F
    -- G
    (Up, G) -> G_sharp
    (Down, G) -> F_sharp
    -- G#
    (Up, G_sharp) -> A
    (Down, G_sharp) -> A
    -- A
    (Up, A) -> A_sharp
    (Down, A) -> G_sharp
    -- A#
    (Up, A_sharp) -> B
    (Down, A_sharp) -> A
    -- B
    (Up, B) -> C
    (Down, B) -> A_sharp
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
step : StepDirection -> Interval -> Note -> Note
step dir s note =
  let
    doHalfStep = halfStep dir
  in
    case s of
      Half       -> doHalfStep note
      Whole      -> doHalfStep note |> doHalfStep
      MinorThird -> doHalfStep note |> doHalfStep |> doHalfStep
      MajorThird -> doHalfStep note |> doHalfStep |> doHalfStep |> doHalfStep

{-| Returns a list of all of the notes up from a give `note` -}
applySteps : List Interval -> Note -> List Note
applySteps steps note =
  case List.foldl (\s (prev, result) -> ((step Up s prev), (step Up s prev :: result))) (note, []) steps of
    (_, result) -> List.reverse result

{-| Return a list of the notes that comprise a `chord` -}
notesForChord : Chord -> List Note
notesForChord chord =
  case chord of
    -- TODO(wpcarro): Use the Position to rotate the chord n times
    Chord (note, chordType, _) -> note :: applySteps (intervalsForChordType chordType) note

{-| Return the scale for a given `key` -}
notesForKey : Key -> List Note
notesForKey key =
  case key of
    Key (note, mode) -> applySteps (intervalsForMode mode) note

{-| Return a list of all of the chords that we know about. -}
allChords : List Chord
allChords =
  let notes = [ C
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
    |> List.Extra.andThen (\chordPosition -> [Chord (note, chordType, chordPosition)])))
