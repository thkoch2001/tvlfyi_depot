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


type View
    = Habits


type HabitType
    = Daily
    | Weekly
    | Yearly


type alias Habit =
    String


type alias Model =
    { isLoading : Bool
    , view : View
    , dayOfWeek : Maybe Weekday
    , completed : Set Int
    }


{-| The initial state for the application.
-}
init : ( Model, Cmd Msg )
init =
    ( { isLoading = False
      , view = Habits
      , dayOfWeek = Nothing
      , completed = Set.empty
      }
    , Date.today |> Task.perform ReceiveDate
    )


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ completed } as model) =
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
            ( { model | dayOfWeek = Just Sun }, Cmd.none )

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
