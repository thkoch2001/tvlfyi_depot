module Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra


type Strategy
    = Always String
    | When Bool String
    | If Bool String String


class : List Strategy -> Attribute msg
class classes =
    classes
        |> List.map
            (\strategy ->
                case strategy of
                    Always x ->
                        Just x

                    When True x ->
                        Just x

                    When False _ ->
                        Nothing

                    If True x _ ->
                        Just x

                    If False _ x ->
                        Just x
            )
        |> Maybe.Extra.values
        |> String.join " "
        |> Html.Attributes.class
