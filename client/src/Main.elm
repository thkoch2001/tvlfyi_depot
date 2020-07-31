module Main exposing (main)

import Admin
import Browser
import Html exposing (..)
import Login
import Manager
import State
import Url
import User


viewForRoute : State.Route -> (State.Model -> Html State.Msg)
viewForRoute route =
    case route of
        State.Login ->
            Login.render

        State.UserHome ->
            User.render

        State.ManagerHome ->
            Manager.render

        State.AdminHome ->
            Admin.render


view : State.Model -> Browser.Document State.Msg
view model =
    { title = "TripPlanner"
    , body =
        [ case ( model.session, model.route ) of
            -- Redirect to /login when someone is not authenticated.
            -- TODO(wpcarro): We should ensure that /login shows in the URL
            -- bar.
            ( Nothing, _ ) ->
                Login.render model

            ( Just session, Nothing ) ->
                Login.render model

            -- Authenticated
            ( Just session, Just route ) ->
                if State.isAuthorized session.role route then
                    viewForRoute route model

                else
                    text "Access denied. You are not authorized to be here. Evacuate the area immediately"
        ]
    }


main =
    Browser.application
        { init = State.init
        , onUrlChange = State.UrlChanged
        , onUrlRequest = State.LinkClicked
        , subscriptions = \_ -> Sub.none
        , update = State.update
        , view = view
        }
