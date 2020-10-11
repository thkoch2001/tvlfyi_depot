module Habits exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import State
import Time exposing (Weekday(..))
import UI
import Utils exposing (Strategy(..))


morning : List State.Habit
morning =
    List.map
        (\( duration, x ) ->
            { label = x
            , habitType = State.Morning
            , minutesDuration = duration
            }
        )
        [ ( 1, "Make bed" )
        , ( 2, "Brush teeth" )
        , ( 10, "Shower" )
        , ( 1, "Do push-ups" )
        , ( 10, "Meditate" )
        ]


evening : List State.Habit
evening =
    List.map
        (\( duration, x ) ->
            { label = x
            , habitType = State.Evening
            , minutesDuration = duration
            }
        )
        [ ( 30, "Read" )
        , ( 1, "Record in State.Habit Journal" )
        ]


monday : List ( Int, String )
monday =
    [ ( 90, "Bikram Yoga @ 17:00" )
    ]


tuesday : List ( Int, String )
tuesday =
    [ ( 90, "Bikram Yoga @ 18:00" )
    ]


wednesday : List ( Int, String )
wednesday =
    [ ( 5, "Shave" )
    , ( 90, "Bikram Yoga @ 17:00" )
    ]


thursday : List ( Int, String )
thursday =
    []


friday : List ( Int, String )
friday =
    [ ( 60, "Bikram Yoga @ 17:00" )
    , ( 3, "Take-out trash" )
    , ( 60, "Shop for groceries" )
    ]


saturday : List ( Int, String )
saturday =
    [ ( 60, "Warm Yin Yoga @ 15:00" )
    ]


sunday : List ( Int, String )
sunday =
    [ ( 1, "Shampoo" )
    , ( 5, "Shave" )
    , ( 1, "Trim nails" )
    , ( 1, "Combine trash cans" )
    , ( 10, "Mop tile and wood floors" )
    , ( 10, "Laundry" )
    , ( 5, "Vacuum bedroom" )
    , ( 5, "Dust surfaces" )
    , ( 5, "Clean mirrors" )
    , ( 5, "Clean desk" )
    ]


payday : List State.Habit
payday =
    List.map
        (\( duration, x ) ->
            { label = x
            , habitType = State.Payday
            , minutesDuration = duration
            }
        )
        [ ( 1, "Ensure \"Emergency\" fund has a balance of 1000 GBP" )
        , ( 1, "Open \"finances_2020\" Google Sheet" )
        , ( 1, "Settle up with Mimi on TransferWise" )
        , ( 1, "Adjust GBP:USD exchange rate" )
        , ( 1, "Adjust \"Stocks (after tax)\" to reflect amount Google sent" )
        , ( 1, "Add remaining cash to \"Carryover (cash)\"" )
        , ( 1, "Adjust \"Paycheck\" to reflect amount Google sent" )
        , ( 5, "In the \"International Xfer\" table, send \"Xfer amount\" from Monzo to USAA" )
        , ( 10, "Go to an ATM and extract the amount in \"ATM withdrawal\"" )
        , ( 0, "Await the TransferWise transaction to complete and pay MyFedLoan in USD" )
        ]


firstOfTheMonth : List State.Habit
firstOfTheMonth =
    List.map
        (\( duration, x ) ->
            { label = x
            , habitType = State.FirstOfTheMonth
            , minutesDuration = duration
            }
        )
        [ ( 10, "Create habit template in journal" )
        , ( 30, "Assess previous month's performance" )
        , ( 5, "Register for Bikram Yoga classes" )
        ]


firstOfTheYear : List State.Habit
firstOfTheYear =
    List.map
        (\( duration, x ) ->
            { label = x
            , habitType = State.FirstOfTheYear
            , minutesDuration = duration
            }
        )
        [ ( 60, "Write a post mortem for the previous year" )
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
    let
        toHabit =
            List.map
                (\( duration, x ) ->
                    { label = x
                    , habitType = State.DayOfWeek
                    , minutesDuration = duration
                    }
                )
    in
    case weekday of
        Mon ->
            toHabit monday

        Tue ->
            toHabit tuesday

        Wed ->
            toHabit wednesday

        Thu ->
            toHabit thursday

        Fri ->
            toHabit friday

        Sat ->
            toHabit saturday

        Sun ->
            toHabit sunday


timeRemaining : Set Int -> List State.Habit -> Int
timeRemaining completed habits =
    habits
        |> List.indexedMap
            (\i { minutesDuration } ->
                if Set.member i completed then
                    0

                else
                    minutesDuration
            )
        |> List.sum


render : State.Model -> Html State.Msg
render { today, visibleDayOfWeek, completed } =
    case visibleDayOfWeek of
        Nothing ->
            p [] [ text "Unable to display habits because we do not know what day of the week it is." ]

        Just weekday ->
            div
                [ Utils.class
                    [ Always "container mx-auto py-6 px-6"
                    , When (today /= visibleDayOfWeek) "pt-20"
                    ]
                ]
                [ header []
                    [ if today /= visibleDayOfWeek then
                        div [ class "text-center w-full bg-blue-600 text-white fixed top-0 left-0 px-3 py-4" ]
                            [ p [ class "py-2 inline pr-5" ]
                                [ text "As you are not viewing today's habits, the UI is in read-only mode" ]
                            , UI.button
                                [ class "bg-blue-200 px-4 py-2 rounded text-blue-600 text-xs font-bold"
                                , onClick State.ViewToday
                                ]
                                [ text "View Today's Habits" ]
                            ]

                      else
                        text ""
                    , div [ class "flex center" ]
                        [ UI.button
                            [ class "w-1/4 text-gray-500"
                            , onClick State.ViewPrevious
                            ]
                            [ text "‹ previous" ]
                        , h1 [ class "font-bold text-blue-500 text-3xl text-center w-full" ]
                            [ text (weekdayName weekday) ]
                        , UI.button
                            [ class "w-1/4 text-gray-500"
                            , onClick State.ViewNext
                            ]
                            [ text "next ›" ]
                        ]
                    ]
                , if today == visibleDayOfWeek then
                    p [ class "text-center" ]
                        [ let
                            t =
                                timeRemaining completed (habitsFor weekday)
                          in
                          if t == 0 then
                            text "Nothing to do!"

                          else
                            text
                                ((weekday
                                    |> habitsFor
                                    |> timeRemaining completed
                                    |> String.fromInt
                                 )
                                    ++ " minutes remaining"
                                )
                        ]

                  else
                    text ""
                , ul []
                    (weekday
                        |> habitsFor
                        |> List.indexedMap
                            (\i { label, minutesDuration } ->
                                let
                                    isCompleted =
                                        Set.member i completed
                                in
                                li [ class "text-xl list-disc ml-6" ]
                                    [ if today == visibleDayOfWeek then
                                        UI.button
                                            [ class "py-5 px-3"
                                            , onClick (State.ToggleHabit i)
                                            ]
                                            [ span
                                                [ Utils.class
                                                    [ Always "text-white pt-1 px-2 rounded"
                                                    , If isCompleted "bg-gray-400" "bg-blue-500"
                                                    ]
                                                ]
                                                [ text (String.fromInt minutesDuration ++ " mins") ]
                                            , p
                                                [ Utils.class
                                                    [ Always "inline pl-3"
                                                    , When isCompleted "line-through text-gray-400"
                                                    ]
                                                ]
                                                [ text label ]
                                            ]

                                      else
                                        UI.button
                                            [ class "py-5 px-3 cursor-not-allowed"
                                            , onClick State.DoNothing
                                            ]
                                            [ text label ]
                                    ]
                            )
                    )
                , footer [ class "text-sm text-center text-gray-500 fixed bottom-0 left-0 w-full py-4" ]
                    [ p [] [ text "This app is brought to you by William Carroll." ]
                    , p [] [ text "Client: Elm; Server: n/a" ]
                    ]
                ]
