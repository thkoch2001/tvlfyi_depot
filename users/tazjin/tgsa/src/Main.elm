module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = UrlChange String


update msg model =
    model


view model =
    div []
        [ p [] [ text "tgsa: 1. paste telegram link 2. receive BBCode 3. post " ]
        , input [ placeholder "Telegram message URL", onInput UrlChange ] []
        ]
