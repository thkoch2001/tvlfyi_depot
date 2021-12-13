module Piano exposing (render)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Theory
import UI


type alias KeyMarkup a =
    { offset : Int
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


naturalThickness : Int
naturalThickness =
    105


accidentalThickness : Int
accidentalThickness =
    round (toFloat naturalThickness / 2.0)


{-| Convert an integer into its pixel representation for CSS.
-}
pixelate : Int -> String
pixelate x =
    String.fromInt x ++ "px"


{-| Return the markup for either a white or a black key.
-}
pianoKey : KeyMarkup a
pianoKey { offset, isHighlit, note, isRootNote } =
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

        { keyLength, keyThickness, keyColor, offsetEdge, extraClasses } =
            case Theory.keyClass note of
                Theory.Natural ->
                    { keyLength = "w-screen"
                    , keyThickness = naturalThickness
                    , keyColor = natColor
                    , offsetEdge = "top"
                    , extraClasses = []
                    }

                Theory.Accidental ->
                    { keyLength = "w-2/3"
                    , keyThickness = accidentalThickness
                    , keyColor = accColor
                    , offsetEdge = "top"
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
        , class keyLength
        , style "height" (pixelate keyThickness)
        , style offsetEdge (String.fromInt offset ++ "px")
        , class <| String.join " " (List.concat [ sharedClasses, extraClasses ])
        ]
        []


{-| A section of the piano consisting of all twelve notes.
-}
keys :
    { start : Theory.Note
    , end : Theory.Note
    , highlitNotes : List Theory.Note
    , rootNote : Maybe Theory.Note
    }
    -> List (Html a)
keys { start, end, highlitNotes, rootNote } =
    let
        isHighlit note =
            List.member note highlitNotes

        spacing prevOffset prev curr =
            case ( Theory.keyClass prev, Theory.keyClass curr ) of
                ( Theory.Natural, Theory.Accidental ) ->
                    prevOffset + naturalThickness - round (toFloat accidentalThickness / 2)

                ( Theory.Accidental, Theory.Natural ) ->
                    prevOffset + round (toFloat accidentalThickness / 2)

                ( Theory.Natural, Theory.Natural ) ->
                    prevOffset + naturalThickness

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
            { start = Theory.G3
            , end = Theory.C6
            , rootNote = chord |> Maybe.map .note
            , highlitNotes =
                chord
                    |> Maybe.andThen Theory.notesForChord
                    |> Maybe.withDefault []
            }
        )
