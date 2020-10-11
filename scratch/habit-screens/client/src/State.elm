module State exposing (..)

import Date
import Set exposing (Set)
import Task
import Time exposing (Weekday(..))


type Msg
    = DoNothing
    | SetView View
    | ReceiveDate Date.Date
    | ToggleHabit Int
    | MaybeAdjustWeekday
    | ViewToday
    | ViewPrevious
    | ViewNext
    | ClearAll


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
    { label : String
    , habitType : HabitType
    , minutesDuration : Int
    }


type alias Model =
    { isLoading : Bool
    , view : View
    , today : Maybe Weekday
    , completed : Set Int
    , visibleDayOfWeek : Maybe Weekday
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
                | today = Just (Date.weekday x)
                , visibleDayOfWeek = Just (Date.weekday x)
              }
            , Cmd.none
            )

        ToggleHabit i ->
            ( { model
                | completed =
                    if Set.member i completed then
                        Set.remove i completed

                    else
                        Set.insert i completed
              }
            , Cmd.none
            )

        MaybeAdjustWeekday ->
            ( model, Date.today |> Task.perform ReceiveDate )

        ViewToday ->
            ( { model | visibleDayOfWeek = today }, Cmd.none )

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
