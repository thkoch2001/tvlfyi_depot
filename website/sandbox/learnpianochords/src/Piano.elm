module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Theory


{-| On mobile phones, the keyboard displays vertically.
-}
type Direction
    = Horizontal
    | Vertical


type alias KeyMarkup a =
    { offset : Int
    , isHighlit : Bool
    , note : Theory.Note
    , direction : Direction
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
naturalWidth : Direction -> Int
naturalWidth direction =
    case direction of
        Vertical ->
            -- Right now, I'm designing this specifically for my Google Pixel 4
            -- phone, which has a screen width of 1080px.
            1080

        Horizontal ->
            45


{-| Pixel height of the white keys.
-}
naturalHeight : Direction -> Int
naturalHeight direction =
    case direction of
        Vertical ->
            -- Right now, I'm designing this specifically for my Google Pixel 4
            -- phone, which has a screen height of 2280px. 2280 / 21
            -- (i.e. no. natural keys) ~= 108
            108

        Horizontal ->
            250


{-| Pixel width of the black keys.
-}
accidentalWidth : Direction -> Int
accidentalWidth direction =
    case direction of
        Vertical ->
            round (toFloat (naturalWidth direction) * 0.6)

        Horizontal ->
            round (toFloat (naturalWidth direction) * 0.4)


{-| Pixel height of the black keys.
-}
accidentalHeight : Direction -> Int
accidentalHeight direction =
    case direction of
        Vertical ->
            round (toFloat (naturalHeight direction) * 0.63)

        Horizontal ->
            round (toFloat (naturalHeight direction) * 0.63)


{-| Return the markup for either a white or a black key.
-}
pianoKey : KeyMarkup a
pianoKey { offset, isHighlit, note, direction } =
    let
        sharedClasses =
            [ "box-border" ]

        { keyWidth, keyHeight, keyColor, offsetEdge, extraClasses } =
            case ( Theory.keyClass note, direction ) of
                ( Theory.Natural, Vertical ) ->
                    { keyWidth = naturalWidth Vertical
                    , keyHeight = naturalHeight Vertical
                    , keyColor = "white"
                    , offsetEdge = "top"
                    , extraClasses = []
                    }

                ( Theory.Natural, Horizontal ) ->
                    { keyWidth = naturalWidth Horizontal
                    , keyHeight = naturalHeight Horizontal
                    , keyColor = "white"
                    , offsetEdge = "left"
                    , extraClasses = []
                    }

                ( Theory.Accidental, Vertical ) ->
                    { keyWidth = accidentalWidth Vertical
                    , keyHeight = accidentalHeight Vertical
                    , keyColor = "black"
                    , offsetEdge = "top"
                    , extraClasses = [ "z-10" ]
                    }

                ( Theory.Accidental, Horizontal ) ->
                    { keyWidth = accidentalWidth Horizontal
                    , keyHeight = accidentalHeight Horizontal
                    , keyColor = "black"
                    , offsetEdge = "left"
                    , extraClasses = [ "z-10" ]
                    }
    in
    div
        [ style "background-color"
            (if isHighlit then
                "red"

             else
                keyColor
            )
        , style "border-top" "1px solid black"
        , style "border-bottom" "1px solid black"
        , style "border-left" "1px solid black"
        , style "border-right" "1px solid black"
        , style "width" (pixelate keyWidth)
        , style "height" (pixelate keyHeight)
        , style "position" "absolute"
        , style offsetEdge (String.fromInt offset ++ "px")
        , class <| String.join " " (List.concat [ sharedClasses, extraClasses ])
        ]
        []


{-| A section of the piano consisting of all twelve notes.
-}
keys : Direction -> Theory.Note -> Theory.Note -> List Theory.Note -> List (Html a)
keys direction start end highlight =
    let
        isHighlit note =
            List.member note highlight

        spacing prevOffset prev curr =
            case ( Theory.keyClass prev, Theory.keyClass curr, direction ) of
                -- Horizontal
                ( Theory.Natural, Theory.Accidental, Horizontal ) ->
                    prevOffset + naturalWidth direction - round (toFloat (accidentalWidth direction) / 2)

                ( Theory.Accidental, Theory.Natural, Horizontal ) ->
                    prevOffset + round (toFloat (accidentalWidth direction) / 2)

                ( Theory.Natural, Theory.Natural, Horizontal ) ->
                    prevOffset + naturalWidth direction

                -- Vertical
                ( Theory.Natural, Theory.Accidental, Vertical ) ->
                    prevOffset + naturalHeight direction - round (toFloat (accidentalHeight direction) / 2)

                ( Theory.Accidental, Theory.Natural, Vertical ) ->
                    prevOffset + round (toFloat (accidentalHeight direction) / 2)

                ( Theory.Natural, Theory.Natural, Vertical ) ->
                    prevOffset + naturalHeight direction

                -- This pattern should never hit.
                _ ->
                    prevOffset

        ( _, _, notes ) =
            Theory.notesFromRange start end
                |> List.foldl
                    (\curr ( prevOffset, prev, result ) ->
                        case ( prevOffset, prev ) of
                            ( Nothing, Nothing ) ->
                                ( Just 0
                                , Just curr
                                , pianoKey
                                    { offset = 0
                                    , isHighlit = List.member curr highlight
                                    , note = curr
                                    , direction = direction
                                    }
                                    :: result
                                )

                            ( Just po, Just p ) ->
                                let
                                    offset =
                                        spacing po p curr
                                in
                                ( Just offset
                                , Just curr
                                , pianoKey
                                    { offset = offset
                                    , isHighlit = List.member curr highlight
                                    , note = curr
                                    , direction = direction
                                    }
                                    :: result
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
        (keys Vertical start end highlight |> List.reverse |> List.repeat 1 |> List.concat)
