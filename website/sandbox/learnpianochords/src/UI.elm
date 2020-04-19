module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Responsive
import Tailwind


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
        [ class (Tailwind.use <| List.concat [ buttonClasses, classes ])
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
        [ class (Tailwind.use <| List.concat [ buttonClasses, classes ])
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
        [ class (Tailwind.use <| List.concat [ inputClasses, classes ])
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
            [ "fixed"
            , "top-0"
            , "left-0"
            , "block"
            , "z-30"
            , "w-screen"
            , "h-screen"
            , Tailwind.if_ isVisible "opacity-100" "opacity-0"
            ]
    in
    button
        [ classes |> Tailwind.use |> class
        , style "background-color" "rgba(0,0,0,0.30)"
        , onClick handleClick
        ]
        [ h1
            [ style "-webkit-text-stroke-width" "2px"
            , style "-webkit-text-stroke-color" "black"
            , class <|
                Tailwind.use
                    [ "transform"
                    , "-rotate-90"
                    , "text-white"
                    , "font-mono"
                    , Responsive.h1
                    ]
            ]
            [ text label ]
        ]
