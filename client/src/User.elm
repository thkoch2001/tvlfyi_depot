module User exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import RemoteData
import State
import Tailwind
import UI
import Utils


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
        , UI.simpleButton
            { label = "Logout"
            , handleClick = State.AttemptLogout
            }
        , case model.logoutError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error logging out"
                    , body = Utils.explainHttpError e
                    }
        ]
