module Tempo exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Responsive
import Tailwind
import UI


type alias Props msg =
    { tempo : Int
    , handleInput : String -> msg
    }


render : Props msg -> Html msg
render { tempo, handleInput } =
    div [ class "text-center" ]
        [ p
            [ [ "py-10"
              , Responsive.h2
              ]
                |> Tailwind.use
                |> class
            ]
            [ text (String.fromInt tempo ++ " BPM") ]
        , UI.textField
            { placeholderText = "Set tempo..."
            , handleInput = handleInput
            , classes = []
            }
        ]
