module Manager exposing (render)

import Array
import Common
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData
import State
import Tailwind
import UI
import Utils


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
                [ h1 []
                    [ UI.header 2 ("Welcome back, " ++ session.username ++ "!")
                    , UI.textButton
                        { label = "Logout"
                        , handleClick = State.AttemptLogout
                        }
                    , allUsers model
                    , Common.allErrors model
                    ]
                ]
        )
