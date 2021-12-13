module Tailwind exposing (..)

{-| Functions to make Tailwind development in Elm even more pleasant.
-}


{-| Conditionally use `class` selection when `condition` is true.
-}
when : Bool -> String -> String
when condition class =
    if condition then
        class

    else
        ""


if_ : Bool -> String -> String -> String
if_ condition whenTrue whenFalse =
    if condition then
        whenTrue

    else
        whenFalse


use : List String -> String
use styles =
    String.join " " styles
