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


render : State.Model -> Html State.Msg
render model =
    case model.session of
        Nothing ->
            text "You are unauthorized to view this page."

        Just session ->
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
                    , UI.simpleButton
                        { label = "Logout"
                        , handleClick = State.AttemptLogout
                        }
                    , Common.allErrors model
                    ]
                ]
