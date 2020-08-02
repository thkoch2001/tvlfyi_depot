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


renderEditTrip : State.Model -> State.Trip -> Html State.Msg
renderEditTrip model trip =
    li []
        [ div []
            [ UI.textField
                { handleInput = State.UpdateEditTripDestination
                , inputId = "edit-trip-destination"
                , inputValue = model.editTripDestination
                , pholder = "Destination"
                }
            , UI.textField
                { handleInput = State.UpdateEditTripComment
                , inputId = "edit-trip-comment"
                , inputValue = model.editTripComment
                , pholder = "Comment"
                }
            ]
        , div []
            [ UI.baseButton
                { enabled =
                    case model.updateTripStatus of
                        RemoteData.Loading ->
                            False

                        _ ->
                            True
                , extraClasses = []
                , label =
                    case model.updateTripStatus of
                        RemoteData.Loading ->
                            "Saving..."

                        _ ->
                            "Save"
                , handleClick =
                    State.AttemptUpdateTrip
                        { username = trip.username
                        , destination = trip.destination
                        , startDate = trip.startDate
                        }
                        { username = trip.username
                        , destination = model.editTripDestination
                        , startDate = trip.startDate
                        , endDate = trip.endDate
                        , comment = model.editTripComment
                        }
                }
            , UI.simpleButton
                { label = "Cancel"
                , handleClick = State.CancelEditTrip
                }
            ]
        ]


renderTrip : Date.Date -> State.Trip -> Html State.Msg
renderTrip today trip =
    li
        [ [ "py-2" ]
            |> Tailwind.use
            |> class
        ]
        [ if Date.compare today trip.startDate == GT then
            UI.paragraph
                (String.fromInt (Date.diff Date.Days trip.startDate today)
                    ++ " days until you're travelling to "
                    ++ trip.destination
                    ++ " for "
                    ++ String.fromInt
                        (Date.diff
                            Date.Days
                            trip.startDate
                            trip.endDate
                        )
                    ++ " days."
                )

          else
            UI.paragraph
                (String.fromInt (Date.diff Date.Days today trip.endDate)
                    ++ " days ago you returned from your trip to "
                    ++ trip.destination
                )
        , UI.paragraph ("\"" ++ trip.comment ++ "\"")
        , UI.wrapNoPrint
            (UI.textButton
                { label = "Edit"
                , handleClick = State.EditTrip trip
                }
            )
        , UI.wrapNoPrint
            (UI.textButton
                { label = "Delete"
                , handleClick = State.AttemptDeleteTrip trip
                }
            )
        ]


trips : State.Model -> Html State.Msg
trips model =
    div []
        [ UI.header 3 "Your Trips"
        , case model.trips of
            RemoteData.NotAsked ->
                UI.paragraph "Somehow we've reached the user home page without requesting your trips data. Please report this to our engineering team at bugs@tripplaner.tld"

            RemoteData.Loading ->
                UI.paragraph "Loading your trips..."

            RemoteData.Failure e ->
                UI.paragraph ("Error: " ++ Utils.explainHttpError e)

            RemoteData.Success xs ->
                case model.todaysDate of
                    Nothing ->
                        text ""

                    Just today ->
                        div [ [ "mb-10" ] |> Tailwind.use |> class ]
                            [ ul [ [ "my-4" ] |> Tailwind.use |> class ]
                                (xs
                                    |> List.sortWith (\x y -> Date.compare y.startDate x.startDate)
                                    |> List.map
                                        (\trip ->
                                            case model.editingTrip of
                                                Nothing ->
                                                    renderTrip today trip

                                                Just x ->
                                                    if x == trip then
                                                        renderEditTrip model trip

                                                    else
                                                        renderTrip today trip
                                        )
                                )
                            , UI.wrapNoPrint
                                (UI.simpleButton
                                    { label = "Print iternary"
                                    , handleClick = State.PrintPage
                                    }
                                )
                            ]
        ]


render : State.Model -> Html State.Msg
render model =
    Common.withSession model
        (\session ->
            div
                [ class
                    ([ "container"
                     , "mx-auto"
                     , "text-center"
                     ]
                        |> Tailwind.use
                    )
                ]
                [ UI.wrapNoPrint (UI.header 2 ("Welcome, " ++ session.username ++ "!"))
                , UI.wrapNoPrint (createTrip model)
                , trips model
                , UI.wrapNoPrint
                    (UI.textButton
                        { label = "Logout"
                        , handleClick = State.AttemptLogout
                        }
                    )
                , Common.allErrors model
                ]
        )
