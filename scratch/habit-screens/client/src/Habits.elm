module Habits exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set
import State
import Time exposing (Weekday(..))


morning : List State.Habit
morning =
    [ "Make bed"
    , "Brush teeth"
    , "Shower"
    , "Do push-ups"
    , "Meditate"
    ]


evening : List State.Habit
evening =
    [ "Read (30 minutes)"
    , "Record in State.Habit Journal"
    ]


monday : List State.Habit
monday =
    [ "Bikram Yoga @ 17:00 (90 min)"
    ]


tuesday : List State.Habit
tuesday =
    [ "Bikram Yoga @ 18:00 (90 min)"
    ]


wednesday : List State.Habit
wednesday =
    [ "Shave"
    , "Bikram Yoga @ 17:00 (90 min)"
    ]


thursday : List State.Habit
thursday =
    []


friday : List State.Habit
friday =
    [ "Bikram Yoga @ 17:00 (60 min)"
    , "Take-out trash"
    , "Shop for groceries"
    ]


saturday : List State.Habit
saturday =
    [ "Warm Yin Yoga @ 15:00 (60 min)"
    ]


sunday : List State.Habit
sunday =
    [ "Shampoo"
    , "Shave"
    , "Trim nails"
    , "Combine trash cans"
    , "Mop tile and wood floors"
    , "Laundry"
    , "Vacuum bedroom"
    , "Dust surfaces"
    , "Clean mirrors"
    , "Clean desk"
    ]


weekdayName : Weekday -> String
weekdayName weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


habitsFor : Weekday -> List State.Habit
habitsFor weekday =
    case weekday of
        Mon ->
            monday

        Tue ->
            tuesday

        Wed ->
            wednesday

        Thu ->
            thursday

        Fri ->
            friday

        Sat ->
            saturday

        Sun ->
            sunday


tailwind : List ( String, Bool ) -> Attribute msg
tailwind classes =
    classes
        |> List.filter (\( k, v ) -> v)
        |> List.map (\( k, v ) -> k)
        |> String.join " "
        |> class


render : State.Model -> Html State.Msg
render { today, visibleDayOfWeek, completed } =
    case visibleDayOfWeek of
        Nothing ->
            p [] [ text "Unable to display habits because we do not know what day of the week it is." ]

        Just weekday ->
            div
                [ class "font-sans py-6 px-6"
                , tailwind [ ( "pt-20", today /= visibleDayOfWeek ) ]
                ]
                [ header []
                    [ if today /= visibleDayOfWeek then
                        div [ class "text-center w-full bg-blue-600 text-white fixed top-0 left-0 px-3 py-4" ]
                            [ p [ class "py-2 inline pr-5" ]
                                [ text "As you are not viewing today's habits, the UI is in read-only mode" ]
                            , button
                                [ class "bg-blue-200 px-4 py-2 rounded text-blue-600 text-xs font-bold"
                                , onClick State.ViewToday
                                ]
                                [ text "View Today's Habits" ]
                            ]

                      else
                        text ""
                    , div [ class "flex center" ]
                        [ button
                            [ class "w-1/4 text-gray-500"
                            , onClick State.ViewPrevious
                            ]
                            [ text "‹ previous" ]
                        , h1 [ class "font-bold text-gray-500 text-3xl text-center w-full" ]
                            [ text (weekdayName weekday) ]
                        , button
                            [ class "w-1/4 text-gray-500"
                            , onClick State.ViewNext
                            ]
                            [ text "next ›" ]
                        ]
                    ]
                , ul []
                    (weekday
                        |> habitsFor
                        |> List.indexedMap
                            (\i x ->
                                li [ class "text-xl list-disc ml-6" ]
                                    [ button
                                        [ class "py-5 px-3"
                                        , tailwind
                                            [ ( "line-through", today == visibleDayOfWeek && Set.member i completed )
                                            ]
                                        , onClick
                                            (if today /= visibleDayOfWeek then
                                                State.DoNothing

                                             else
                                                State.ToggleHabit i
                                            )
                                        ]
                                        [ text x ]
                                    ]
                            )
                    )
                , footer [ class "font-mono text-sm text-center text-gray-500 fixed bottom-0 left-0 w-full py-4" ]
                    [ p [] [ text "This app is brought to you by William Carroll." ]
                    , p [] [ text "Client: Elm; Server: n/a" ]
                    ]
                ]
