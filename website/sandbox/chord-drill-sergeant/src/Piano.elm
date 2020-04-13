module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Theory


type alias KeyMarkup a =
    { offset : Int
    , isHighlit : Bool
    , note : Theory.Note
    }
    -> Html a


type alias Props =
    { highlight : List Theory.Note
    , start : Theory.Note
    , end : Theory.Note
    }


{-| Convert an integer into its pixel representation for CSS.
-}
pixelate : Int -> String
pixelate x =
    String.fromInt x ++ "px"


{-| Pixel width of the white keys.
-}
naturalWidth : Int
naturalWidth =
    45


{-| Pixel height of the white keys.
-}
naturalHeight : Int
naturalHeight =
    250


{-| Pixel width of the black keys.
-}
accidentalWidth : Int
accidentalWidth =
    round (toFloat naturalWidth * 0.4)


{-| Pixel height of the black keys.
-}
accidentalHeight : Int
accidentalHeight =
    round (toFloat naturalHeight * 0.63)


{-| These are the white keys on most modern pianos.
-}
natural : KeyMarkup a
natural { offset, isHighlit, note } =
    div
        [ style "background-color"
            (if isHighlit then
                "red"

             else
                "white"
            )
        , style "border-right" "1px solid black"
        , style "border-top" "1px solid black"
        , style "border-bottom" "1px solid black"
        , style "width" (pixelate naturalWidth)
        , style "height" (pixelate naturalHeight)
        , style "position" "absolute"
        , style "left" (String.fromInt offset ++ "px")
        ]
        []


{-| These are the black keys on most modern pianos.
-}
accidental : KeyMarkup a
accidental { offset, isHighlit, note } =
    div
        [ style "background-color"
            (if isHighlit then
                "red"

             else
                "black"
            )
        , style "border-top" "1px solid black"
        , style "border-left" "1px solid black"
        , style "border-right" "1px solid black"
        , style "border-bottom" "1px solid black"
        , style "width" (pixelate accidentalWidth)
        , style "height" (pixelate accidentalHeight)
        , style "position" "absolute"
        , style "left" (String.fromInt offset ++ "px")
        , style "z-index" "1"
        ]
        []


makeKey : List Theory.Note -> Theory.Note -> (Int -> Html a)
makeKey highlight note =
    if Theory.isNatural note then
        \x ->
            natural
                { offset = x
                , isHighlit = List.member note highlight
                , note = note
                }

    else
        \x ->
            accidental
                { offset = x
                , isHighlit = List.member note highlight
                , note = note
                }


{-| A section of the piano consisting of all twelve notes. The name octave
implies eight notes, which most scales (not the blues scale) honor.
-}
octave : Theory.Note -> Theory.Note -> List Theory.Note -> List (Html a)
octave start end highlight =
    let
        isHighlit note =
            List.member note highlight

        spacing prevOffset prev curr =
            case ( Theory.keyClass prev, Theory.keyClass curr ) of
                ( Theory.Natural, Theory.Accidental ) ->
                    -- idk this calculation yet
                    prevOffset + naturalWidth - round (toFloat accidentalWidth / 2)

                ( Theory.Accidental, Theory.Natural ) ->
                    -- accidentalWidth / 2
                    prevOffset + round (toFloat accidentalWidth / 2)

                ( Theory.Natural, Theory.Natural ) ->
                    -- naturalWidth
                    prevOffset + naturalWidth

                -- This pattern should never hit.
                _ ->
                    prevOffset

        ( _, _, notes ) =
            Theory.notesFromRange start end
                |> List.foldl
                    (\curr ( prevOffset, prev, result ) ->
                        case ( prevOffset, prev ) of
                            ( Nothing, Nothing ) ->
                                ( Just 0, Just curr, makeKey highlight curr 0 :: result )

                            ( Just po, Just p ) ->
                                let
                                    offset =
                                        spacing po p curr
                                in
                                ( Just offset
                                , Just curr
                                , makeKey highlight curr offset :: result
                                )

                            -- This pattern should never hit.
                            _ ->
                                ( Nothing, Nothing, [] )
                    )
                    ( Nothing, Nothing, [] )
    in
    List.reverse notes


{-| Return the HTML that renders a piano representation.
-}
render : Props -> Html a
render { highlight, start, end } =
    div [ style "display" "flex" ]
        (octave start end highlight |> List.reverse |> List.repeat 1 |> List.concat)
