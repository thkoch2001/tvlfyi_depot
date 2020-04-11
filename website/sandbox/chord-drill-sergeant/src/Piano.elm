module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Theory

{-| These are the white keys on most modern pianos. -}
natural : Bool -> Html a
natural isHighlit =
  li [ style "background-color" (if isHighlit then "red" else "white")
     , style "height" "20px"
     , style "border-top" "1px solid black"
     , style "border-bottom" "1px solid black"
     ] []

{-| These are the black keys on most modern pianos. -}
accidental : Bool -> Html a
accidental isHighlit =
  li [ style "background-color" (if isHighlit then "red" else "black")
     , style "height" "10px"
     , style "width" "66%"
     ] []

{-| A section of the piano consisting of all twelve notes. The name octave
implies eight notes, which most scales (not the blues scale) honor. -}
octave : List Theory.Note -> List (Html a)
octave highlight =
  let
    isHighlit note = List.member note highlight
  in
    [ natural (isHighlit Theory.C)
    , accidental (isHighlit Theory.C_sharp)
    , natural (isHighlit Theory.D)
    , accidental (isHighlit Theory.D_sharp)
    , natural (isHighlit Theory.E)
    , natural (isHighlit Theory.F)
    , accidental (isHighlit Theory.F_sharp)
    , natural (isHighlit Theory.G)
    , accidental (isHighlit Theory.G_sharp)
    , natural (isHighlit Theory.A)
    , accidental (isHighlit Theory.A_sharp)
    , natural (isHighlit Theory.B)
    ]

indexForNote : Theory.Note -> Int
indexForNote note =
  case note of
    Theory.C       -> 0
    Theory.C_sharp -> 1
    Theory.D       -> 2
    Theory.D_sharp -> 3
    Theory.E       -> 4
    Theory.F       -> 5
    Theory.F_sharp -> 6
    Theory.G       -> 7
    Theory.G_sharp -> 8
    Theory.A       -> 9
    Theory.A_sharp -> 10
    Theory.B       -> 11

{-| Return the HTML that renders a piano representation. -}
render : { highlight : List Theory.Note } -> Html a
render {highlight} =
  ul [ style "width" "100px"
     , style "list-style" "none"
     ] (octave highlight |> List.reverse |> List.repeat 1 |> List.concat)
