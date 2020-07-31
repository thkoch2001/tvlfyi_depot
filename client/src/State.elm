module State exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Process
import RemoteData exposing (WebData)
import Shared
import Task
import Time
import Url
import Url.Builder as UrlBuilder
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)
import Utils



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


type Msg
    = DoNothing
    | UpdateUsername String
    | UpdateEmail String
    | UpdatePassword String
    | UpdateRole String
    | UpdateAdminTab AdminTab
    | ClearErrors
    | ToggleLoginForm
      -- SPA
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- Outbound network
    | AttemptGetUsers
    | AttemptSignUp
    | AttemptLogin
    | AttemptLogout
    | AttemptDeleteUser String
      -- Inbound network
    | GotUsers (WebData AllUsers)
    | GotSignUp (Result Http.Error Session)
    | GotLogin (Result Http.Error Session)
    | GotLogout (Result Http.Error String)
    | GotDeleteUser (Result Http.Error String)


type Route
    = Login
    | UserHome
    | ManagerHome
    | AdminHome


type Role
    = User
    | Manager
    | Admin


type alias AllUsers =
    { user : List String
    , manager : List String
    , admin : List String
    }


type alias Session =
    { role : Role
    , username : String
    }


type alias Review =
    { rowid : Int
    , content : String
    , rating : Int
    , user : String
    , dateOfVisit : String
    }


type alias Reviews =
    { hi : Maybe Review
    , lo : Maybe Review
    , all : List Review
    }


type AdminTab
    = Users


type LoginTab
    = LoginForm
    | SignUpForm


type alias Model =
    { route : Maybe Route
    , url : Url.Url
    , key : Nav.Key
    , session : Maybe Session
    , username : String
    , email : String
    , password : String
    , role : Maybe Role
    , users : WebData AllUsers
    , adminTab : AdminTab
    , loginTab : LoginTab
    , loginError : Maybe Http.Error
    , logoutError : Maybe Http.Error
    , signUpError : Maybe Http.Error
    , deleteUserError : Maybe Http.Error
    }



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------


roleToString : Role -> String
roleToString role =
    case role of
        User ->
            "user"

        Manager ->
            "manager"

        Admin ->
            "admin"


endpoint : List String -> List UrlBuilder.QueryParameter -> String
endpoint =
    UrlBuilder.crossOrigin Shared.serverOrigin


decodeRole : JD.Decoder Role
decodeRole =
    let
        toRole : String -> JD.Decoder Role
        toRole s =
            case s of
                "user" ->
                    JD.succeed User

                "manager" ->
                    JD.succeed Manager

                "admin" ->
                    JD.succeed Admin

                _ ->
                    JD.succeed User
    in
    JD.string |> JD.andThen toRole


decodeSession : JD.Decoder Session
decodeSession =
    JD.map2
        Session
        (JD.field "role" decodeRole)
        (JD.field "username" JD.string)


encodeLoginRequest : String -> String -> JE.Value
encodeLoginRequest username password =
    JE.object
        [ ( "username", JE.string username )
        , ( "password", JE.string password )
        ]


login : String -> String -> Cmd Msg
login username password =
    Utils.postWithCredentials
        { url = endpoint [ "login" ] []
        , body = Http.jsonBody (encodeLoginRequest username password)
        , expect = Http.expectJson GotLogin decodeSession
        }


logout : Cmd Msg
logout =
    Utils.getWithCredentials
        { url = endpoint [ "logout" ] []
        , expect = Http.expectString GotLogout
        }


signUp :
    { username : String
    , email : String
    , password : String
    }
    -> Cmd Msg
signUp { username, email, password } =
    Utils.postWithCredentials
        { url = endpoint [ "accounts" ] []
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "username", JE.string username )
                    , ( "email", JE.string username )
                    , ( "password", JE.string password )
                    , ( "role", JE.string "user" )
                    ]
                )
        , expect = Http.expectJson GotSignUp decodeSession
        }


deleteUser : String -> Cmd Msg
deleteUser username =
    Utils.deleteWithCredentials
        { url = endpoint [ "user", username ] []
        , expect = Http.expectString GotDeleteUser
        }


decodeReview : JD.Decoder Review
decodeReview =
    JD.map5
        Review
        (JD.field "rowid" JD.int)
        (JD.field "content" JD.string)
        (JD.field "rating" JD.int)
        (JD.field "user" JD.string)
        (JD.field "timestamp" JD.string)


fetchUsers : Cmd Msg
fetchUsers =
    Utils.getWithCredentials
        { url = endpoint [ "all-usernames" ] []
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> GotUsers)
                (JD.map3
                    AllUsers
                    (JD.field "user" (JD.list JD.string))
                    (JD.field "manager" (JD.list JD.string))
                    (JD.field "admin" (JD.list JD.string))
                )
        }


sleepAndClearErrors : Cmd Msg
sleepAndClearErrors =
    Process.sleep 4000
        |> Task.perform (\_ -> ClearErrors)


isAuthorized : Role -> Route -> Bool
isAuthorized role route =
    case ( role, route ) of
        ( User, _ ) ->
            True

        ( Manager, _ ) ->
            True

        ( Admin, _ ) ->
            True


homeRouteForRole : Role -> String
homeRouteForRole role =
    case role of
        User ->
            "/user"

        Manager ->
            "/manager"

        Admin ->
            "/admin"


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Login (s "topic")
        , map UserHome (s "user")
        , map ManagerHome (s "manager")
        , map AdminHome (s "admin")
        ]


{-| The initial state for the application.
-}
init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = Nothing
      , url = url
      , key = key
      , session = Nothing
      , username = ""
      , email = ""
      , password = ""
      , role = Nothing
      , users = RemoteData.NotAsked
      , adminTab = Users
      , loginTab = LoginForm
      , loginError = Nothing
      , logoutError = Nothing
      , signUpError = Nothing
      , deleteUserError = Nothing
      }
    , Cmd.none
    )


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        UpdateUsername x ->
            ( { model | username = x }, Cmd.none )

        UpdatePassword x ->
            ( { model | password = x }, Cmd.none )

        UpdateEmail x ->
            ( { model | email = x }, Cmd.none )

        UpdateAdminTab x ->
            ( { model | adminTab = x }, Cmd.none )

        UpdateRole x ->
            let
                maybeRole =
                    case x of
                        "user" ->
                            Just User

                        "manager" ->
                            Just Manager

                        "admin" ->
                            Just Admin

                        _ ->
                            Nothing
            in
            ( { model | role = maybeRole }, Cmd.none )

        ClearErrors ->
            ( { model
                | loginError = Nothing
                , logoutError = Nothing
                , signUpError = Nothing
                , deleteUserError = Nothing
              }
            , Cmd.none
            )

        ToggleLoginForm ->
            ( { model
                | loginTab =
                    case model.loginTab of
                        LoginForm ->
                            SignUpForm

                        SignUpForm ->
                            LoginForm
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Url.Parser.parse routeParser url
            in
            case route of
                Just UserHome ->
                    ( { model
                        | url = url
                        , route = route
                      }
                    , Cmd.none
                    )

                Just ManagerHome ->
                    case model.session of
                        Nothing ->
                            ( { model
                                | url = url
                                , route = route
                              }
                            , Cmd.none
                            )

                        Just session ->
                            ( { model
                                | url = url
                                , route = route
                              }
                            , Cmd.none
                            )

                Just AdminHome ->
                    ( { model
                        | url = url
                        , route = route
                        , users = RemoteData.Loading
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | url = url
                        , route = route
                      }
                    , Cmd.none
                    )

        -- GET /all-usernames
        AttemptGetUsers ->
            ( { model | users = RemoteData.Loading }, fetchUsers )

        GotUsers xs ->
            ( { model | users = xs }, Cmd.none )

        -- DELETE /user/:username
        AttemptDeleteUser username ->
            ( model, deleteUser username )

        GotDeleteUser result ->
            case result of
                Ok _ ->
                    ( model, fetchUsers )

                Err e ->
                    ( { model | deleteUserError = Just e }
                    , sleepAndClearErrors
                    )

        -- /create-account
        AttemptSignUp ->
            ( model
            , signUp
                { username = model.username
                , email = model.email
                , password = model.password
                }
            )

        GotSignUp result ->
            case result of
                Ok session ->
                    ( { model | session = Just session }
                    , Nav.pushUrl model.key (homeRouteForRole session.role)
                    )

                Err x ->
                    ( { model | signUpError = Just x }
                    , sleepAndClearErrors
                    )

        -- /login
        AttemptLogin ->
            ( model, login model.username model.password )

        GotLogin result ->
            case result of
                Ok session ->
                    ( { model | session = Just session }
                    , Nav.pushUrl model.key (homeRouteForRole session.role)
                    )

                Err x ->
                    ( { model | loginError = Just x }
                    , sleepAndClearErrors
                    )

        -- / logout
        AttemptLogout ->
            ( model, logout )

        GotLogout result ->
            case result of
                Ok _ ->
                    ( { model | session = Nothing }
                    , Nav.pushUrl model.key "/login"
                    )

                Err e ->
                    ( { model | logoutError = Just e }
                    , sleepAndClearErrors
                    )
