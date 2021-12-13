module State exposing (..)

import Date exposing (Date)
import Set exposing (Set)
import Task
import Time exposing (Weekday(..))


type alias WeekdayLabel =
    String


type alias HabitLabel =
    String


type Msg
    = DoNothing
    | SetView View
    | ReceiveDate Date
    | ToggleHabit WeekdayLabel HabitLabel
    | MaybeAdjustWeekday
    | ViewToday
    | ViewPrevious
    | ViewNext
    | ClearAll
    | ToggleMorning
    | ToggleEvening


type View
    = Habits


type HabitType
    = Morning
    | Evening
    | DayOfWeek
    | Payday
    | FirstOfTheMonth
    | FirstOfTheYear


type alias Habit =
    { label : HabitLabel
    , habitType : HabitType
    , minutesDuration : Int
    }


type alias CompletedHabits =
    Set ( WeekdayLabel, HabitLabel )


type alias Model =
    { isLoading : Bool
    , view : View
    , today : Maybe Date
    , completed : CompletedHabits
    , visibleDayOfWeek : Maybe Weekday
    , includeMorning : Bool
    , includeEvening : Bool
    }


previousDay : Weekday -> Weekday
previousDay weekday =
    case weekday of
        Mon ->
            Sun

        Tue ->
            Mon

        Wed ->
            Tue

        Thu ->
            Wed

        Fri ->
            Thu

        Sat ->
            Fri

        Sun ->
            Sat


nextDay : Weekday -> Weekday
nextDay weekday =
    case weekday of
        Mon ->
            Tue

        Tue ->
            Wed

        Wed ->
            Thu

        Thu ->
            Fri

        Fri ->
            Sat

        Sat ->
            Sun

        Sun ->
            Mon


{-| The initial state for the application.
-}
init : ( Model, Cmd Msg )
init =
    ( { isLoading = False
      , view = Habits
      , today = Nothing
      , completed = Set.empty
      , visibleDayOfWeek = Nothing
      , includeMorning = True
      , includeEvening = True
      }
    , Date.today |> Task.perform ReceiveDate
    )


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ today, visibleDayOfWeek, completed } as model) =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        SetView x ->
            ( { model
                | view = x
                , isLoading = True
              }
            , Cmd.none
            )

        ReceiveDate x ->
            ( { model
                | today = Just x
                , visibleDayOfWeek = Just (Date.weekday x)
              }
            , Cmd.none
            )

        ToggleHabit weekdayLabel habitLabel ->
            ( { model
                | completed =
                    if Set.member ( weekdayLabel, habitLabel ) completed then
                        Set.remove ( weekdayLabel, habitLabel ) completed

                    else
                        Set.insert ( weekdayLabel, habitLabel ) completed
              }
            , Cmd.none
            )

        MaybeAdjustWeekday ->
            ( model, Date.today |> Task.perform ReceiveDate )

        ViewToday ->
            ( { model | visibleDayOfWeek = today |> Maybe.map Date.weekday }, Cmd.none )

        ViewPrevious ->
            ( { model
                | visibleDayOfWeek = visibleDayOfWeek |> Maybe.map previousDay
              }
            , Cmd.none
            )

        ViewNext ->
            ( { model
                | visibleDayOfWeek = visibleDayOfWeek |> Maybe.map nextDay
              }
            , Cmd.none
            )

        ClearAll ->
            ( { model | completed = Set.empty }, Cmd.none )

        ToggleMorning ->
            ( { model | includeMorning = not model.includeMorning }, Cmd.none )

        ToggleEvening ->
            ( { model | includeEvening = not model.includeEvening }, Cmd.none )
