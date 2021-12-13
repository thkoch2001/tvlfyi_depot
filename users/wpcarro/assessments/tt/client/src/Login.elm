module Login exposing (render)

import Common
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State
import Tailwind
import UI
import Utils


googleSignIn : Html State.Msg
googleSignIn =
    div
        [ class "g-signin2"
        , attribute "onsuccess" "onSignIn"
        , onClick State.GoogleSignIn
        ]
        []


loginForm : State.Model -> Html State.Msg
loginForm model =
    div
        [ [ "w-full"
          , "max-w-xs"
          , "mx-auto"
          ]
            |> Tailwind.use
            |> class
        ]
        [ div
            [ [ "bg-white"
              , "shadow-md"
              , "rounded"
              , "px-8"
              , "pt-6"
              , "pb-8"
              , "mb-4"
              , "text-left"
              ]
                |> Tailwind.use
                |> class
            ]
            [ div [ [ "text-center", "pb-6" ] |> Tailwind.use |> class ]
                [ UI.textButton
                    { handleClick = State.ToggleLoginForm
                    , label =
                        case model.loginTab of
                            State.LoginForm ->
                                "Switch to sign up"

                            State.SignUpForm ->
                                "Switch to login"
                    }
                ]
            , div
                [ [ "mb-4" ] |> Tailwind.use |> class ]
                [ UI.label_ { for_ = "username", text_ = "Username" }
                , UI.textField
                    { inputId = "Username"
                    , pholder = "Username"
                    , handleInput = State.UpdateUsername
                    , inputValue = model.username
                    }
                ]
            , case model.loginTab of
                State.LoginForm ->
                    text ""

                State.SignUpForm ->
                    div
                        [ [ "mb-4" ] |> Tailwind.use |> class ]
                        [ UI.label_ { for_ = "email", text_ = "Email" }
                        , input
                            [ [ "shadow"
                              , "appearance-none"
                              , "border"
                              , "rounded"
                              , "w-full"
                              , "py-2"
                              , "px-3"
                              , "text-gray-700"
                              , "leading-tight"
                              , "focus:outline-none"
                              , "focus:shadow-outline"
                              ]
                                |> Tailwind.use
                                |> class
                            , id "email"
                            , placeholder "who@domain.tld"
                            , onInput State.UpdateEmail
                            ]
                            []
                        ]
            , div
                [ [ "mb-4" ] |> Tailwind.use |> class ]
                [ UI.label_ { for_ = "password", text_ = "Password" }
                , input
                    [ [ "shadow"
                      , "appearance-none"
                      , "border"
                      , "rounded"
                      , "w-full"
                      , "py-2"
                      , "px-3"
                      , "text-gray-700"
                      , "leading-tight"
                      , "focus:outline-none"
                      , "focus:shadow-outline"
                      ]
                        |> Tailwind.use
                        |> class
                    , id "password"
                    , type_ "password"
                    , placeholder "******************"
                    , onInput State.UpdatePassword
                    ]
                    []
                ]
            , case model.loginTab of
                State.LoginForm ->
                    div [ [ "flex", "space-around" ] |> Tailwind.use |> class ]
                        [ UI.simpleButton
                            { handleClick = State.AttemptLogin
                            , label = "Login"
                            }
                        , div [ [ "pl-4" ] |> Tailwind.use |> class ] [ googleSignIn ]
                        ]

                State.SignUpForm ->
                    if
                        List.all identity
                            [ String.length model.username > 0
                            , String.length model.email > 0
                            , String.length model.password > 0
                            ]
                    then
                        div []
                            [ UI.simpleButton
                                { handleClick = State.AttemptSignUp
                                , label = "Sign up"
                                }
                            ]

                    else
                        UI.disabledButton { label = "Sign up" }
            ]
        ]


login :
    State.Model
    -> Html State.Msg
login model =
    div
        [ [ "text-center"
          , "py-20"
          , "bg-gray-200"
          , "h-screen"
          ]
            |> Tailwind.use
            |> class
        ]
        [ UI.header 3 "Welcome to Trip Planner"
        , loginForm model
        , Common.allErrors model
        ]


logout : State.Model -> Html State.Msg
logout model =
    div
        [ [ "text-center"
          , "py-20"
          , "bg-gray-200"
          , "h-screen"
          ]
            |> Tailwind.use
            |> class
        ]
        [ UI.header 3 "Looks like you're already signed in..."
        , UI.simpleButton
            { label = "Logout"
            , handleClick = State.AttemptLogout
            }
        , Common.allErrors model
        ]


render : State.Model -> Html State.Msg
render model =
    case model.session of
        Nothing ->
            login model

        Just x ->
            logout model
