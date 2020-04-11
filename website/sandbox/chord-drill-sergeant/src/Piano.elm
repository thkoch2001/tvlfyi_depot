module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Theory

{-| Convert an integer into its pixel representation for CSS. -}
pixelate : Int -> String
pixelate x = String.fromInt x ++ "px"

{-| Pixel width of the white keys. -}
naturalWidth : Int
naturalWidth = 40

{-| Pixel height of the white keys. -}
naturalHeight : Int
naturalHeight = 200

{-| Pixel width of the black keys. -}
accidentalWidth : Int
accidentalWidth = round (toFloat naturalWidth * 0.7)

{-| Pixel height of the black keys. -}
accidentalHeight : Int
accidentalHeight = round (toFloat naturalHeight * 0.6)

{-| These are the white keys on most modern pianos. -}
natural : Int -> Bool -> Html a
natural offset isHighlit =
  div [ style "background-color" (if isHighlit then "red" else "white")
      , style "border-right" "1px solid black"
      , style "border-top" "1px solid black"
      , style "border-bottom" "1px solid black"
      , style "width" (pixelate naturalWidth)
      , style "height" (pixelate naturalHeight)
      , style "position" "absolute"
      , style "left" ((String.fromInt offset) ++ "px")
      ] []

{-| These are the black keys on most modern pianos. -}
accidental : Int -> Bool -> Html a
accidental offset isHighlit =
  div [ style "background-color" (if isHighlit then "red" else "black")
      , style "border-top" "1px solid black"
      , style "border-left" "1px solid black"
      , style "border-right" "1px solid black"
      , style "border-bottom" "1px solid black"
      , style "width" (pixelate accidentalWidth)
      , style "height" (pixelate accidentalHeight)
      , style "position" "absolute"
      , style "left" ((String.fromInt offset) ++ "px")
      , style "z-index" "1"
      ] []

{-| A section of the piano consisting of all twelve notes. The name octave
implies eight notes, which most scales (not the blues scale) honor. -}
octave : List Theory.Note -> List (Html a)
octave highlight =
  let
    isHighlit note = List.member note highlight
  in
    [ natural    0    (isHighlit Theory.C)
    , accidental 25   (isHighlit Theory.C_sharp)
    , natural    40   (isHighlit Theory.D)
    , accidental 65   (isHighlit Theory.D_sharp)
    , natural    80   (isHighlit Theory.E)
    , natural    120  (isHighlit Theory.F)
    , accidental 145  (isHighlit Theory.F_sharp)
    , natural    160  (isHighlit Theory.G)
    , accidental 185  (isHighlit Theory.G_sharp)
    , natural    200  (isHighlit Theory.A)
    , accidental 225  (isHighlit Theory.A_sharp)
    , natural    240  (isHighlit Theory.B)
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
  div [ style "display" "flex" ] (octave highlight |> List.reverse |> List.repeat 1 |> List.concat)
