module Tempo exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UI


type alias Props msg =
    { tempo : Int
    , handleInput : String -> msg
    }


render : Props msg -> Html msg
render { tempo, handleInput } =
    div [ class "text-center" ]
        [ p [ class "text-5xl py-10" ] [ text (String.fromInt tempo ++ " BPM") ]
        , UI.textField
            { placeholderText = "Set tempo..."
            , handleInput = handleInput
            , classes = []
            }
        ]
