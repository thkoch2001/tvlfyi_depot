module Login exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import State
import Tailwind
import UI
import Utils


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
                    UI.simpleButton { handleClick = State.AttemptLogin, label = "Login" }

                State.SignUpForm ->
                    if String.length model.username > 0 && String.length model.email > 0 && String.length model.password > 0 then
                        UI.simpleButton { handleClick = State.AttemptSignUp, label = "Sign up" }

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
        , case model.loginError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error logging in"
                    , body = Utils.explainHttpError e
                    }
        , case model.signUpError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error creating account"
                    , body = Utils.explainHttpError e
                    }
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
        , case model.logoutError of
            Nothing ->
                text ""

            Just e ->
                UI.errorBanner
                    { title = "Error logging out"
                    , body = Utils.explainHttpError e
                    }
        ]


render : State.Model -> Html State.Msg
render model =
    case model.session of
        Nothing ->
            login model

        Just x ->
            logout model
