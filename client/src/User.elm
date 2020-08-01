module User exposing (render)

import Common
import Date
import DatePicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as ME
import RemoteData
import State
import Tailwind
import UI
import Utils


createTrip : State.Model -> Html State.Msg
createTrip model =
    div []
        [ UI.header 3 "Plan Upcoming Trip"
        , UI.textField
            { pholder = "Where are you going?"
            , inputId = "destination"
            , handleInput = State.UpdateTripDestination
            , inputValue = model.tripDestination
            }
        , div [ [ "flex" ] |> Tailwind.use |> class ]
            [ UI.datePicker
                { mDate = model.tripStartDate
                , prompt = "Set departure date"
                , prefix = "Departure: "
                , picker = model.startDatePicker
                , onUpdate = State.UpdateTripStartDate
                }
            , UI.datePicker
                { mDate = model.tripEndDate
                , prompt = "Set return date"
                , prefix = "Return: "
                , picker = model.endDatePicker
                , onUpdate = State.UpdateTripEndDate
                }
            ]
        , UI.textField
            { pholder = "Comments?"
            , inputId = "comment"
            , handleInput = State.UpdateTripComment
            , inputValue = model.tripComment
            }
        , UI.baseButton
            { enabled =
                List.all
                    identity
                    [ String.length model.tripDestination > 0
                    , String.length model.tripComment > 0
                    , ME.isJust model.tripStartDate
                    , ME.isJust model.tripEndDate
                    ]
            , extraClasses = [ "my-4" ]
            , handleClick =
                case ( model.tripStartDate, model.tripEndDate ) of
                    ( Nothing, _ ) ->
                        State.DoNothing

                    ( _, Nothing ) ->
                        State.DoNothing

                    ( Just startDate, Just endDate ) ->
                        State.AttemptCreateTrip startDate endDate
            , label = "Schedule trip"
            }
        ]


trips : State.Model -> Html msg
trips model =
    div []
        [ UI.header 3 "Upcoming Trips"
        , case model.trips of
            RemoteData.NotAsked ->
                UI.paragraph "Somehow we've reached the user home page without requesting your trips data. Please report this to our engineering team at bugs@tripplaner.tld"

            RemoteData.Loading ->
                UI.paragraph "Loading your trips..."

            RemoteData.Failure e ->
                UI.paragraph ("Error: " ++ Utils.explainHttpError e)

            RemoteData.Success xs ->
                ul []
                    (xs
                        |> List.map
                            (\trip ->
                                li
                                    [ [ "py-2" ]
                                        |> Tailwind.use
                                        |> class
                                    ]
                                    [ text
                                        (Date.toIsoString trip.startDate
                                            ++ " - "
                                            ++ Date.toIsoString trip.endDate
                                            ++ " -> "
                                            ++ trip.destination
                                        )
                                    ]
                            )
                    )
        ]


render : State.Model -> Html State.Msg
render model =
    div
        [ class
            ([ "container"
             , "mx-auto"
             , "text-center"
             ]
                |> Tailwind.use
            )
        ]
        [ UI.header 2 ("Welcome, " ++ model.username ++ "!")
        , createTrip model
        , trips model
        , UI.textButton
            { label = "Logout"
            , handleClick = State.AttemptLogout
            }
        , Common.allErrors model
        ]
