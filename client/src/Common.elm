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
