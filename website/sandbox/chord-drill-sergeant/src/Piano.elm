module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

{-| These are the white keys on most modern pianos. -}
natural : Html a
natural =
  li [ style "background-color" "white"
     , style "height" "20px"
     , style "border-top" "1px solid black"
     ] []

{-| These are the black keys on most modern pianos. -}
accidental : Html a
accidental =
  li [ style "background-color" "black"
     , style "height" "10px"
     , style "width" "66%"
     ] []

{-| A section of the piano consisting of all twelve notes. The name octave
implies eight notes, which most scales (not the blues scale) honor. -}
octave : List (Html a)
octave = [ natural
         , accidental
         , natural
         , accidental
         , natural
         , natural
         , accidental
         , natural
         , accidental
         , natural
         , accidental
         , natural
         ]

{-| Return the HTML that renders a piano representation. -}
render : Html a
render =
  ul [ style "width" "100px"
     , style "list-style" "none"
     ] (octave |> List.repeat 3 |> List.concat)
