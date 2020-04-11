module Tempo exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Props msg =
  { tempo : Int
  , handleIncrease : msg
  , handleDecrease : msg
  , handleInput : String -> msg
  }

render : Props msg -> Html msg
render {tempo, handleIncrease, handleDecrease, handleInput} =
  div [] [ p [] [ text ((String.fromInt tempo) ++ " BPM") ]
         , button [ onClick handleDecrease ] [ text "Slower" ]
         , input [ onInput handleInput
                 , placeholder "Set tempo..."
                 ] []
         , button [ onClick handleIncrease ] [ text "Faster" ]
         ]
