module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Responsive


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
            , "lg:py-6"
            , "px-20"
            , "lg:px-12"
            , "rounded-lg"
            , Responsive.h2
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
            , "py-8"
            , "lg:py-5"
            , "px-10"
            , "lg:px-6"
            , Responsive.h2
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
            [ "w-full"
            , "py-10"
            , "lg:py-6"
            , "px-16"
            , "lg:px-10"
            , "border"
            , "rounded-lg"
            , Responsive.h2
            ]
    in
    input
        [ class (tw <| List.concat [ inputClasses, classes ])
        , onInput handleInput
        , placeholder placeholderText
        ]
        []


overlayButton :
    { label : String
    , handleClick : msg
    , isVisible : Bool
    }
    -> Html msg
overlayButton { label, handleClick, isVisible } =
    let
        classes =
            [ "bg-red-600"
            , "absolute"
            , "top-0"
            , "left-0"
            , "h-screen"
            , "w-screen"
            , "z-30"
            ]

        extraClasses =
            if isVisible then
                [ "opacity-100" ]

            else
                [ "opacity-0" ]
    in
    button
        [ List.concat [ classes, extraClasses ] |> tw |> class
        , style "background-color" "rgba(0,0,0,0.30)"
        , onClick handleClick
        ]
        [ h1
            [ style "-webkit-text-stroke-width" "2px"
            , style "-webkit-text-stroke-color" "black"
            , class <|
                tw
                    [ "transform"
                    , "-rotate-90"
                    , "text-white"
                    , "font-mono"
                    , Responsive.h1
                    ]
            ]
            [ text label ]
        ]
