module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Theory
import UI


{-| On mobile phones, the keyboard displays vertically.
-}
type Direction
    = Horizontal
    | Vertical


type alias KeyMarkup a =
    { offset : Int
    , direction : Direction
    , isHighlit : Bool
    , note : Theory.Note
    , isRootNote : Bool
    }
    -> Html a


type alias Props =
    { chord : Maybe Theory.Chord
    , firstNote : Theory.Note
    , lastNote : Theory.Note
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
            1080

        Horizontal ->
            45


{-| Pixel height of the white keys.
-}
naturalHeight : Direction -> Int
naturalHeight direction =
    case direction of
        Vertical ->
            130

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
pianoKey { offset, isHighlit, note, direction, isRootNote } =
    let
        { natColor, accColor, hiColor, rootColor } =
            { natColor = "bg-white"
            , accColor = "bg-black"
            , hiColor = "bg-red-400"
            , rootColor = "bg-red-600"
            }

        sharedClasses =
            [ "box-border"
            , "absolute"
            , "border"
            , "border-black"
            ]

        { keyWidth, keyHeight, keyColor, offsetEdge, extraClasses } =
            case ( Theory.keyClass note, direction ) of
                ( Theory.Natural, Vertical ) ->
                    { keyWidth = naturalWidth Vertical
                    , keyHeight = naturalHeight Vertical
                    , keyColor = natColor
                    , offsetEdge = "top"
                    , extraClasses = []
                    }

                ( Theory.Natural, Horizontal ) ->
                    { keyWidth = naturalWidth Horizontal
                    , keyHeight = naturalHeight Horizontal
                    , keyColor = natColor
                    , offsetEdge = "left"
                    , extraClasses = []
                    }

                ( Theory.Accidental, Vertical ) ->
                    { keyWidth = accidentalWidth Vertical
                    , keyHeight = accidentalHeight Vertical
                    , keyColor = accColor
                    , offsetEdge = "top"
                    , extraClasses = [ "z-10" ]
                    }

                ( Theory.Accidental, Horizontal ) ->
                    { keyWidth = accidentalWidth Horizontal
                    , keyHeight = accidentalHeight Horizontal
                    , keyColor = accColor
                    , offsetEdge = "left"
                    , extraClasses = [ "z-10" ]
                    }
    in
    div
        [ class
            (case ( isHighlit, isRootNote ) of
                ( False, _ ) ->
                    keyColor

                ( True, True ) ->
                    rootColor

                ( True, False ) ->
                    hiColor
            )
        , style "width" (pixelate keyWidth)
        , style "height" (pixelate keyHeight)
        , style offsetEdge (String.fromInt offset ++ "px")
        , class <| String.join " " (List.concat [ sharedClasses, extraClasses ])
        ]
        []


{-| A section of the piano consisting of all twelve notes.
-}
keys :
    { direction : Direction
    , start : Theory.Note
    , end : Theory.Note
    , highlitNotes : List Theory.Note
    , rootNote : Maybe Theory.Note
    }
    -> List (Html a)
keys { direction, start, end, highlitNotes, rootNote } =
    let
        isHighlit note =
            List.member note highlitNotes

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
                |> List.reverse
                |> List.foldl
                    (\curr ( prevOffset, prev, result ) ->
                        case ( prevOffset, prev ) of
                            ( Nothing, Nothing ) ->
                                ( Just 0
                                , Just curr
                                , pianoKey
                                    { offset = 0
                                    , isHighlit = List.member curr highlitNotes
                                    , note = curr
                                    , direction = direction
                                    , isRootNote =
                                        rootNote
                                            |> Maybe.map (\x -> x == curr)
                                            |> Maybe.withDefault False
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
                                    , isHighlit = List.member curr highlitNotes
                                    , note = curr
                                    , direction = direction
                                    , isRootNote =
                                        rootNote
                                            |> Maybe.map (\x -> x == curr)
                                            |> Maybe.withDefault False
                                    }
                                    :: result
                                )

                            -- This pattern should never hit.
                            _ ->
                                ( Nothing, Nothing, [] )
                    )
                    ( Nothing, Nothing, [] )
    in
    notes


{-| Return the HTML that renders a piano representation.
-}
render : Props -> Html a
render { chord } =
    div [ style "display" "flex" ]
        (keys
            { direction = Vertical
            , start = Theory.G3
            , end = Theory.C6
            , rootNote = chord |> Maybe.map .note
            , highlitNotes =
                chord
                    |> Maybe.andThen Theory.notesForChord
                    |> Maybe.withDefault []
            }
        )
