module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Color
    = Primary
    | Secondary


bgForColor : Color -> String
bgForColor color =
    case color of
        Primary ->
            "bg-gray-600"

        Secondary ->
            "bg-gray-300"


textForColor : Color -> String
textForColor color =
    case color of
        Primary ->
            "text-white"

        Secondary ->
            "text-black"


tw : List String -> String
tw styles =
    String.join " " styles


simpleButton :
    { label : String
    , handleClick : msg
    , color : Color
    , classes : List String
    }
    -> Html msg
simpleButton { label, handleClick, color, classes } =
    let
        buttonClasses =
            [ bgForColor color
            , textForColor color
            , "py-10"
            , "px-20"
            , "text-5xl"
            , "rounded-lg"
            ]
    in
    button
        [ class (tw <| List.concat [ buttonClasses, classes ])
        , onClick handleClick
        ]
        [ text label ]


textToggleButton :
    { label : String
    , handleClick : msg
    , classes : List String
    , toggled : Bool
    }
    -> Html msg
textToggleButton { label, toggled, handleClick, classes } =
    let
        ( textColor, textTreatment ) =
            if toggled then
                ( "text-red-600", "underline" )

            else
                ( "text-black", "no-underline" )

        buttonClasses =
            [ textColor
            , textTreatment
            , "py-10"
            , "px-10"
            , "text-5xl"
            ]
    in
    button
        [ class (tw <| List.concat [ buttonClasses, classes ])
        , onClick handleClick
        ]
        [ text label ]


textField :
    { placeholderText : String
    , handleInput : String -> msg
    , classes : List String
    }
    -> Html msg
textField { placeholderText, handleInput, classes } =
    let
        inputClasses =
            [ "text-5xl"
            , "w-full"
            , "py-10"
            , "px-16"
            , "border"
            , "rounded-lg"
            ]
    in
    input
        [ class (tw <| List.concat [ inputClasses, classes ])
        , onInput handleInput
        , placeholder placeholderText
        ]
        []
