module Admin exposing (render)

import Common
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData
import State
import Tailwind
import UI
import Utils


allTrips : State.Model -> Html State.Msg
allTrips model =
    case model.trips of
        RemoteData.NotAsked ->
            UI.absentData { handleFetch = State.AttemptGetTrips }

        RemoteData.Loading ->
            UI.paragraph "Loading..."

        RemoteData.Failure e ->
            UI.paragraph ("Error: " ++ Utils.explainHttpError e)

        RemoteData.Success xs ->
            ul []
                (xs
                    |> List.map
                        (\trip ->
                            li []
                                [ UI.paragraph (Date.toIsoString trip.startDate ++ " - " ++ Date.toIsoString trip.endDate ++ ", " ++ trip.username ++ " is going " ++ trip.destination)
                                , UI.textButton
                                    { label = "delete"
                                    , handleClick = State.AttemptDeleteTrip trip
                                    }
                                ]
                        )
                )


allUsers : State.Model -> Html State.Msg
allUsers model =
    case model.accounts of
        RemoteData.NotAsked ->
            UI.absentData { handleFetch = State.AttemptGetAccounts }

        RemoteData.Loading ->
            UI.paragraph "Loading..."

        RemoteData.Failure e ->
            UI.paragraph ("Error: " ++ Utils.explainHttpError e)

        RemoteData.Success xs ->
            ul []
                (xs
                    |> List.map
                        (\account ->
                            li []
                                [ UI.paragraph
                                    (account.username
                                        ++ " - "
                                        ++ State.roleToString account.role
                                    )
                                , UI.textButton
                                    { label = "delete"
                                    , handleClick = State.AttemptDeleteAccount account.username
                                    }
                                ]
                        )
                )


users : List String -> Html State.Msg
users xs =
    ul []
        (xs
            |> List.map
                (\x ->
                    li [ [ "py-4", "flex" ] |> Tailwind.use |> class ]
                        [ p [ [ "flex-1" ] |> Tailwind.use |> class ] [ text x ]
                        , div [ [ "flex-1" ] |> Tailwind.use |> class ]
                            [ UI.simpleButton
                                { label = "Delete"
                                , handleClick = State.AttemptDeleteAccount x
                                }
                            ]
                        ]
                )
        )


render : State.Model -> Html State.Msg
render model =
    div
        [ [ "container"
          , "mx-auto"
          , "text-center"
          ]
            |> Tailwind.use
            |> class
        ]
        [ UI.header 2 "Welcome!"
        , div []
            [ UI.textButton
                { label = "Logout"
                , handleClick = State.AttemptLogout
                }
            ]
        , div [ [ "py-3" ] |> Tailwind.use |> class ]
            [ case model.adminTab of
                State.Accounts ->
                    UI.textButton
                        { label = "Switch to trips"
                        , handleClick = State.UpdateAdminTab State.Trips
                        }

                State.Trips ->
                    UI.textButton
                        { label = "Switch to accounts"
                        , handleClick = State.UpdateAdminTab State.Accounts
                        }
            ]
        , case model.adminTab of
            State.Accounts ->
                allUsers model

            State.Trips ->
                allTrips model
        , Common.allErrors model
        ]
