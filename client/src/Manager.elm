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
                    , Common.allErrors model
                    ]
                ]
        )
