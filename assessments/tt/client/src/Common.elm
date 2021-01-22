module Common exposing (..)

import Html exposing (..)
import Maybe.Extra as ME
import State
import UI
import Utils


allErrors : State.Model -> Html State.Msg
allErrors model =
    div []
        (State.allErrors
            model
            |> List.map
                (\( mError, title ) ->
                    case mError of
                        Nothing ->
                            text ""

                        Just err ->
                            UI.errorBanner
                                { title = title
                                , body = Utils.explainHttpError err
                                }
                )
        )


withSession : State.Model -> (State.Session -> Html State.Msg) -> Html State.Msg
withSession model renderWithSession =
    case model.session of
        Nothing ->
            div [] [ UI.paragraph "You need a valid session to view this page. Please attempt to log in." ]

        Just session ->
            renderWithSession session
