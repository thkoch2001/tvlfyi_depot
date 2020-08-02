module UI exposing (..)

import Date
import DatePicker exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State
import Tailwind


label_ : { for_ : String, text_ : String } -> Html msg
label_ { for_, text_ } =
    label
        [ [ "block"
          , "text-gray-700"
          , "text-sm"
          , "font-bold"
          , "mb-2"
          ]
            |> Tailwind.use
            |> class
        , for for_
        ]
        [ text text_ ]


errorBanner : { title : String, body : String } -> Html msg
errorBanner { title, body } =
    div
        [ [ "text-left"
          , "fixed"
          , "container"
          , "top-0"
          , "mt-6"
          ]
            |> Tailwind.use
            |> class
        , style "left" "50%"

        -- TODO(wpcarro): Consider supporting breakpoints, but for now
        -- don't.
        , style "width" "800px"
        , style "margin-left" "-400px"
        ]
        [ div
            [ [ "bg-red-500"
              , "text-white"
              , "font-bold"
              , "rounded-t"
              , "px-4"
              , "py-2"
              ]
                |> Tailwind.use
                |> class
            ]
            [ text title ]
        , div
            [ [ "border"
              , "border-t-0"
              , "border-red-400"
              , "rounded-b"
              , "bg-red-100"
              , "px-4"
              , "py-3"
              , "text-red-700"
              ]
                |> Tailwind.use
                |> class
            ]
            [ p [] [ text body ] ]
        ]


baseButton :
    { label : String
    , enabled : Bool
    , handleClick : msg
    , extraClasses : List String
    }
    -> Html msg
baseButton { label, enabled, handleClick, extraClasses } =
    button
        [ [ if enabled then
                "bg-blue-500"

            else
                "bg-gray-500"
          , if enabled then
                "hover:bg-blue-700"

            else
                ""
          , if enabled then
                ""

            else
                "cursor-not-allowed"
          , "text-white"
          , "font-bold"
          , "py-1"
          , "shadow-lg"
          , "px-4"
          , "rounded"
          , "focus:outline-none"
          , "focus:shadow-outline"
          ]
            ++ extraClasses
            |> Tailwind.use
            |> class
        , onClick handleClick
        , disabled (not enabled)
        ]
        [ text label ]


simpleButton :
    { label : String
    , handleClick : msg
    }
    -> Html msg
simpleButton { label, handleClick } =
    baseButton
        { label = label
        , enabled = True
        , handleClick = handleClick
        , extraClasses = []
        }


disabledButton :
    { label : String }
    -> Html State.Msg
disabledButton { label } =
    baseButton
        { label = label
        , enabled = False
        , handleClick = State.DoNothing
        , extraClasses = []
        }


textButton :
    { label : String
    , handleClick : msg
    }
    -> Html msg
textButton { label, handleClick } =
    button
        [ [ "text-blue-600"
          , "hover:text-blue-500"
          , "font-bold"
          , "hover:underline"
          , "focus:outline-none"
          ]
            |> Tailwind.use
            |> class
        , onClick handleClick
        ]
        [ text label ]


textField :
    { pholder : String
    , inputId : String
    , handleInput : String -> msg
    , inputValue : String
    }
    -> Html msg
textField { pholder, inputId, handleInput, inputValue } =
    input
        [ [ "shadow"
          , "appearance-none"
          , "border"
          , "rounded"
          , "w-full"
          , "py-2"
          , "px-3"
          , "text-gray-700"
          , "leading-tight"
          , "focus:outline-none"
          , "focus:shadow-outline"
          ]
            |> Tailwind.use
            |> class
        , id inputId
        , value inputValue
        , placeholder pholder
        , onInput handleInput
        ]
        []


toggleButton :
    { toggled : Bool
    , label : String
    , handleEnable : msg
    , handleDisable : msg
    }
    -> Html msg
toggleButton { toggled, label, handleEnable, handleDisable } =
    button
        [ [ if toggled then
                "bg-blue-700"

            else
                "bg-blue-500"
          , "hover:bg-blue-700"
          , "text-white"
          , "font-bold"
          , "py-2"
          , "px-4"
          , "rounded"
          , "focus:outline-none"
          , "focus:shadow-outline"
          ]
            |> Tailwind.use
            |> class
        , onClick
            (if toggled then
                handleDisable

             else
                handleEnable
            )
        ]
        [ text label ]


paragraph : String -> Html msg
paragraph x =
    p [ [ "text-xl" ] |> Tailwind.use |> class ] [ text x ]


header : Int -> String -> Html msg
header which x =
    let
        hStyles =
            case which of
                1 ->
                    [ "text-6xl"
                    , "py-12"
                    ]

                2 ->
                    [ "text-3xl"
                    , "py-6"
                    ]

                _ ->
                    [ "text-2xl"
                    , "py-2"
                    ]
    in
    h1
        [ hStyles
            ++ [ "font-bold"
               , "text-gray-700"
               ]
            |> Tailwind.use
            |> class
        ]
        [ text x ]


link : String -> String -> Html msg
link path label =
    a
        [ href path
        , [ "underline"
          , "text-blue-600"
          , "text-xl"
          ]
            |> Tailwind.use
            |> class
        ]
        [ text label ]


absentData : { handleFetch : msg } -> Html msg
absentData { handleFetch } =
    div []
        [ paragraph "Welp... it looks like you've caught us in a state that we considered impossible: we did not fetch the data upon which this page depends. Maybe you can help us out by clicking the super secret, highly privileged \"Fetch data\" button below (we don't normally show people this)."
        , div [ [ "py-4" ] |> Tailwind.use |> class ]
            [ simpleButton
                { label = "Fetch data"
                , handleClick = handleFetch
                }
            ]
        ]


datePicker :
    { mDate : Maybe Date.Date
    , prompt : String
    , prefix : String
    , picker : DatePicker.DatePicker
    , onUpdate : DatePicker.Msg -> State.Msg
    }
    -> Html State.Msg
datePicker { mDate, prompt, prefix, picker, onUpdate } =
    let
        settings =
            { defaultSettings
                | placeholder = prompt
                , inputClassList =
                    [ ( "text-center", True )
                    , ( "py-2", True )
                    ]
            }
    in
    div [ [ "w-1/2", "py-4", "mx-auto" ] |> Tailwind.use |> class ]
        [ DatePicker.view mDate settings picker |> Html.map onUpdate ]


wrapNoPrint : Html State.Msg -> Html State.Msg
wrapNoPrint component =
    div [ [ "no-print" ] |> Tailwind.use |> class ] [ component ]
