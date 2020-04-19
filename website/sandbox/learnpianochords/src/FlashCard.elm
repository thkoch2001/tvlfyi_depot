module FlashCard exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Responsive
import State
import Tailwind
import Theory


render :
    { chord : Theory.Chord
    , visible : Bool
    }
    -> Html State.Msg
render { chord, visible } =
    let
        classes =
            [ "bg-white"
            , "fixed"
            , "top-0"
            , "left-0"
            , "z-30"
            , "w-screen"
            , "h-screen"
            , Tailwind.if_ visible "opacity-100" "opacity-0"
            ]
    in
    button
        [ classes |> Tailwind.use |> class ]
        [ h1
            [ [ "text-center"
              , "transform"
              , "-rotate-90"
              , Responsive.h1
              ]
                |> Tailwind.use
                |> class
            ]
            [ text (Theory.viewChord chord) ]
        ]
