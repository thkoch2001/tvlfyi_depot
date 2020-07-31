module Admin exposing (render)

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
    case model.users of
        RemoteData.NotAsked ->
            UI.absentData { handleFetch = State.AttemptGetUsers }

        RemoteData.Loading ->
            UI.paragraph "Loading..."

        RemoteData.Failure e ->
            UI.paragraph ("Error: " ++ Utils.explainHttpError e)

        RemoteData.Success xs ->
            div []
                [ UI.header 3 "Admins"
                , users xs.admin
                , UI.header 3 "Managers"
                , users xs.manager
                , UI.header 3 "Users"
                , users xs.user
                ]


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
                                , handleClick = State.AttemptDeleteUser x
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
        [ UI.header 2 "Welcome back!"
        , UI.simpleButton
            { label = "Logout"
            , handleClick = State.AttemptLogout
            }
        , div []
            [ UI.baseButton
                { label = "Switch to users"
                , handleClick = State.UpdateAdminTab State.Users
                , enabled = not (model.adminTab == State.Users)
                , extraClasses = []
                }
            ]
        , case model.adminTab of
            State.Users ->
                allUsers model
        , case model.logoutError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error logging out"
                    , body = Utils.explainHttpError e
                    }
        , case model.deleteUserError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error attempting to delete user"
                    , body = Utils.explainHttpError e
                    }
        ]
