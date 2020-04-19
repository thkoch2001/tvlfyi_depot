module Preferences exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon
import Responsive
import State
import Tempo
import Theory
import UI


selectKey :
    State.Model
    ->
        { relativeMajor : Theory.Key
        , relativeMinor : Theory.Key
        }
    -> Html State.Msg
selectKey model { relativeMajor, relativeMinor } =
    let
        active key =
            List.member key model.whitelistedKeys

        buttonLabel major minor =
            Theory.viewKey major ++ ", " ++ Theory.viewKey minor
    in
    div [ class "flex pt-0" ]
        [ UI.textToggleButton
            { label = buttonLabel relativeMajor relativeMinor
            , handleClick = State.ToggleKey relativeMajor
            , classes = [ "flex-1" ]
            , toggled = active relativeMajor
            }
        ]


chordTypeCheckboxes : List Theory.ChordType -> Html State.Msg
chordTypeCheckboxes chordTypes =
    ul []
        (Theory.allChordTypes
            |> List.map
                (\chordType ->
                    li []
                        [ label [] [ text (Theory.chordTypeName chordType) ]
                        , input
                            [ type_ "checkbox"
                            , onClick (State.ToggleChordType chordType)
                            , checked (List.member chordType chordTypes)
                            ]
                            []
                        ]
                )
        )


inversionCheckboxes : List Theory.ChordInversion -> Html State.Msg
inversionCheckboxes inversions =
    ul []
        (Theory.allInversions
            |> List.map
                (\inversion ->
                    li []
                        [ label [] [ text (Theory.inversionName inversion) ]
                        , input
                            [ type_ "checkbox"
                            , onClick (State.ToggleInversion inversion)
                            , checked (List.member inversion inversions)
                            ]
                            []
                        ]
                )
        )


keyCheckboxes : State.Model -> Html State.Msg
keyCheckboxes model =
    let
        majorKey pitchClass =
            { pitchClass = pitchClass, mode = Theory.MajorMode }

        minorKey pitchClass =
            { pitchClass = pitchClass, mode = Theory.MinorMode }

        circleOfFifths =
            [ ( Theory.C, Theory.A )
            , ( Theory.G, Theory.E )
            , ( Theory.D, Theory.B )
            , ( Theory.A, Theory.F_sharp )
            , ( Theory.E, Theory.C_sharp )
            , ( Theory.B, Theory.G_sharp )
            , ( Theory.F_sharp, Theory.D_sharp )
            , ( Theory.C_sharp, Theory.A_sharp )
            , ( Theory.G_sharp, Theory.F )
            , ( Theory.D_sharp, Theory.C )
            , ( Theory.A_sharp, Theory.G )
            , ( Theory.F, Theory.D )
            ]
    in
    div []
        [ h2
            [ [ "text-gray-500"
              , "text-center"
              , "pt-10"
              , Responsive.h2
              ]
                |> UI.tw
                |> class
            ]
            [ text "Select keys" ]
        , ul []
            (circleOfFifths
                |> List.map
                    (\( major, minor ) ->
                        selectKey model
                            { relativeMajor = majorKey major
                            , relativeMinor = minorKey minor
                            }
                    )
            )
        ]


closePreferences : Html State.Msg
closePreferences =
    button
        [ [ "w-48"
          , "lg:w-32"
          , "h-48"
          , "lg:h-32"
          , "absolute"
          , "right-0"
          , "top-0"
          , "z-10"
          ]
            |> UI.tw
            |> class
        , onClick (State.SetView State.Practice)
        ]
        [ Icon.close ]


render : State.Model -> Html State.Msg
render model =
    div [ class "pt-10 pb-20 px-10" ]
        [ closePreferences
        , Tempo.render
            { tempo = model.tempo
            , handleInput = State.SetTempo
            }
        , case model.practiceMode of
            State.KeyMode ->
                keyCheckboxes model

            State.FineTuneMode ->
                div []
                    [ inversionCheckboxes model.whitelistedInversions
                    , chordTypeCheckboxes model.whitelistedChordTypes
                    ]
        ]
