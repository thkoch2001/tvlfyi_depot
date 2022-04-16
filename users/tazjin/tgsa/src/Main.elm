module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Url
import Url.Parser exposing ((</>), int, string)


type TgLink
    = TgLink String String -- Username & message ID


type alias Model =
    { url : String
    , tgLink : Maybe TgLink
    }


main =
    Browser.sandbox { init = { url = "", tgLink = Nothing }, update = update, view = view }


type Msg
    = UrlChange String


tgLink : Url.Parser.Parser (TgLink -> a) a
tgLink =
    Url.Parser.map TgLink (string </> string)


parseUrl : String -> Maybe TgLink
parseUrl url =
    let
        parsed =
            Url.fromString url
    in
    case parsed of
        Nothing ->
            Nothing

        Just p ->
            if p.host == "t.me" then
                Url.Parser.parse tgLink p

            else
                Nothing


update msg model =
    case msg of
        UrlChange new ->
            { model | url = new, tgLink = parseUrl new }


view model =
    div []
        [ p [] [ text "tgsa: 1. paste telegram link 2. receive BBCode 3. post " ]
        , input [ placeholder "Telegram message URL", onInput UrlChange ] []
        , p []
            [ text (Debug.toString model.tgLink)
            ]
        ]
