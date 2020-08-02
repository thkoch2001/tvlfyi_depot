module State exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date
import DatePicker
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
    | UpdateTripDestination String
    | UpdateTripStartDate DatePicker.Msg
    | UpdateTripEndDate DatePicker.Msg
    | UpdateTripComment String
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
    | AttemptCreateTrip Date.Date Date.Date
      -- Inbound network
    | GotUsers (WebData AllUsers)
    | GotTrips (WebData (List Trip))
    | GotSignUp (Result Http.Error Session)
    | GotLogin (Result Http.Error Session)
    | GotLogout (Result Http.Error String)
    | GotDeleteUser (Result Http.Error String)
    | CreatedTrip (Result Http.Error ())


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


type AdminTab
    = Users


type LoginTab
    = LoginForm
    | SignUpForm


type alias Trip =
    { destination : String
    , startDate : Date.Date
    , endDate : Date.Date
    , comment : String
    }


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
    , startDatePicker : DatePicker.DatePicker
    , endDatePicker : DatePicker.DatePicker
    , tripDestination : String
    , tripStartDate : Maybe Date.Date
    , tripEndDate : Maybe Date.Date
    , tripComment : String
    , trips : WebData (List Trip)
    , adminTab : AdminTab
    , loginTab : LoginTab
    , loginError : Maybe Http.Error
    , logoutError : Maybe Http.Error
    , signUpError : Maybe Http.Error
    , deleteUserError : Maybe Http.Error
    , createTripError : Maybe Http.Error
    }


allErrors : Model -> List ( Maybe Http.Error, String )
allErrors model =
    [ ( model.loginError, "Error attempting to authenticate" )
    , ( model.logoutError, "Error attempting to log out" )
    , ( model.signUpError, "Error attempting to create your account" )
    , ( model.deleteUserError, "Error attempting to delete a user" )
    , ( model.createTripError, "Error attempting to create a trip" )
    ]



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


createTrip :
    { username : String
    , destination : String
    , startDate : Date.Date
    , endDate : Date.Date
    , comment : String
    }
    -> Cmd Msg
createTrip { username, destination, startDate, endDate, comment } =
    Utils.postWithCredentials
        { url = endpoint [ "trips" ] []
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "username", JE.string username )
                    , ( "destination", JE.string destination )
                    , ( "startDate", encodeDate startDate )
                    , ( "endDate", encodeDate endDate )
                    , ( "comment", JE.string comment )
                    ]
                )
        , expect = Http.expectWhatever CreatedTrip
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


encodeDate : Date.Date -> JE.Value
encodeDate date =
    date |> Date.toIsoString |> JE.string


decodeDate : JD.Decoder Date.Date
decodeDate =
    JD.string |> JD.andThen (Date.fromIsoString >> JDE.fromResult)


fetchTrips : Cmd Msg
fetchTrips =
    Utils.getWithCredentials
        { url = endpoint [ "trips" ] []
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> GotTrips)
                (JD.list
                    (JD.map4
                        Trip
                        (JD.field "destination" JD.string)
                        (JD.field "startDate" decodeDate)
                        (JD.field "endDate" decodeDate)
                        (JD.field "comment" JD.string)
                    )
                )
        }


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


{-| Set init to `prod` when going live.
-}
prod : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
prod _ url key =
    let
        ( startDatePicker, startDatePickerCmd ) =
            DatePicker.init

        ( endDatePicker, endDatePickerCmd ) =
            DatePicker.init
    in
    ( { route = Nothing
      , url = url
      , key = key
      , session = Nothing
      , username = ""
      , email = ""
      , password = ""
      , role = Nothing
      , users = RemoteData.NotAsked
      , tripDestination = ""
      , tripStartDate = Nothing
      , tripEndDate = Nothing
      , tripComment = ""
      , trips = RemoteData.NotAsked
      , startDatePicker = startDatePicker
      , endDatePicker = endDatePicker
      , adminTab = Users
      , loginTab = LoginForm
      , loginError = Nothing
      , logoutError = Nothing
      , signUpError = Nothing
      , deleteUserError = Nothing
      , createTripError = Nothing
      }
    , Cmd.batch
        [ Cmd.map UpdateTripStartDate startDatePickerCmd
        , Cmd.map UpdateTripEndDate endDatePickerCmd
        ]
    )


{-| When working on a feature for the UserHome, use this.
-}
userHome : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
userHome flags url key =
    let
        ( model, cmd ) =
            prod flags url key
    in
    ( { model
        | route = Just UserHome
        , session = Just { username = "mimi", role = User }
        , trips =
            RemoteData.Success
                [ { destination = "Barcelona"
                  , startDate = Date.fromCalendarDate 2020 Time.Sep 25
                  , endDate = Date.fromCalendarDate 2020 Time.Oct 5
                  , comment = "Blah"
                  }
                , { destination = "Paris"
                  , startDate = Date.fromCalendarDate 2021 Time.Jan 1
                  , endDate = Date.fromCalendarDate 2021 Time.Feb 1
                  , comment = "Bon voyage!"
                  }
                ]
      }
    , cmd
    )


{-| The initial state for the application.
-}
init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    prod flags url key


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

        UpdateTripDestination x ->
            ( { model | tripDestination = x }, Cmd.none )

        UpdateTripStartDate dpMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update DatePicker.defaultSettings dpMsg model.startDatePicker

                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.tripStartDate
            in
            ( { model
                | tripStartDate = newDate
                , startDatePicker = newDatePicker
              }
            , Cmd.none
            )

        UpdateTripEndDate dpMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update DatePicker.defaultSettings dpMsg model.endDatePicker

                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.tripEndDate
            in
            ( { model
                | tripEndDate = newDate
                , endDatePicker = newDatePicker
              }
            , Cmd.none
            )

        UpdateTripComment x ->
            ( { model | tripComment = x }, Cmd.none )

        ClearErrors ->
            ( { model
                | loginError = Nothing
                , logoutError = Nothing
                , signUpError = Nothing
                , deleteUserError = Nothing
                , createTripError = Nothing
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
                        , trips = RemoteData.Loading
                      }
                    , fetchTrips
                    )

                Just ManagerHome ->
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

        -- GET /accounts
        AttemptGetUsers ->
            ( { model | users = RemoteData.Loading }, fetchUsers )

        GotUsers xs ->
            ( { model | users = xs }, Cmd.none )

        -- DELETE /accounts
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

        -- POST /trips
        AttemptCreateTrip startDate endDate ->
            ( model
            , case model.session of
                Nothing ->
                    Cmd.none

                Just session ->
                    createTrip
                        { username = session.username
                        , destination = model.tripDestination
                        , startDate = startDate
                        , endDate = endDate
                        , comment = model.tripComment
                        }
            )

        CreatedTrip result ->
            case result of
                Ok _ ->
                    ( { model
                        | tripDestination = ""
                        , tripStartDate = Nothing
                        , tripEndDate = Nothing
                        , tripComment = ""
                      }
                    , fetchTrips
                    )

                Err e ->
                    ( { model
                        | createTripError = Just e
                        , tripDestination = ""
                        , tripStartDate = Nothing
                        , tripEndDate = Nothing
                        , tripComment = ""
                      }
                    , sleepAndClearErrors
                    )

        -- POST /accounts
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

        -- GET /trips
        GotTrips xs ->
            ( { model | trips = xs }, Cmd.none )

        -- POST /login
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

        -- GET /logout
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
