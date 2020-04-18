module Overview exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State
import UI


header1 : String -> Html msg
header1 copy =
    h2
        [ [ "text-center", "text-6xl", "pt-24", "pb-12" ]
            |> UI.tw
            |> class
        ]
        [ text copy ]


header2 : String -> Html msg
header2 copy =
    h2 [ [ "text-center", "text-5xl", "pb-10" ] |> UI.tw |> class ]
        [ text copy ]


paragraph : String -> Html msg
paragraph copy =
    p [ [ "text-4xl", "pb-10" ] |> UI.tw |> class ]
        [ text copy ]


sect : { title : String, copy : List String } -> Html msg
sect { title, copy } =
    section [] (header2 title :: (copy |> List.map paragraph))


numberedList : List String -> Html msg
numberedList items =
    ol
        [ [ "list-inside"
          , "list-decimal"
          , "text-4xl"
          ]
            |> UI.tw
            |> class
        ]
        (items |> List.map (\x -> li [ [ "pb-10" ] |> UI.tw |> class ] [ text x ]))


render : State.Model -> Html State.Msg
render model =
    div [ [ "container", "mx-auto" ] |> UI.tw |> class ]
        [ header1 "Welcome to LearnPianoChords.app!"
        , paragraph """
                     Learn Piano Chords helps piano players master chords.
                     """
        , paragraph """
                     Chords are the building blocks songwriters use to create
                     music. Whether you're a performer or songwriter, you need
                     to understand chords to unlock your full musical potential.
                     """
        , paragraph """
                     I think that if practicing is enjoyable, students will
                     practice more. Practice doesn’t make perfect; perfect
                     practice makes perfect.
                     """
        , section []
            [ header2 "Ready to get started?"
            , numberedList
                [ """
                   Sit down at the piano.
                   """
                , """
                   Set the tempo at which you would like to practice.
                   """
                , """
                   Select the key or keys in which you would like to
                   practice.
                   """
                , """
                   When you are ready, close the preferences pane. We will show
                   you the name of a chord, and you should play that chord on
                   the piano.
                 """
                , """
                   If you don't know how to play the chord, toggle the piano
                   viewer to see the notes.
                   """
                , """
                   At any point while you're training, press the screen to pause
                   or resume your practice.
                   """
                ]
            ]
        , div [ [ "text-center", "py-20" ] |> UI.tw |> class ]
            [ UI.simpleButton
                { label = "Let's get started"
                , handleClick = State.SetView State.Preferences
                , color = UI.Secondary
                , classes = []
                }
            ]
        ]
