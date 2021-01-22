port module State exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Date
import DatePicker
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Json.Encode.Extra as JEE
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
    | UpdateEditTripDestination String
    | UpdateEditTripComment String
    | ClearErrors
    | ToggleLoginForm
    | PrintPage
    | GoogleSignIn
    | GoogleSignOut
    | UpdateInviteEmail String
    | UpdateInviteRole (Maybe Role)
    | ReceiveTodaysDate Date.Date
    | EditTrip Trip
    | CancelEditTrip
      -- SPA
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- Outbound network
    | AttemptGetAccounts
    | AttemptGetTrips
    | AttemptSignUp
    | AttemptLogin
    | AttemptLogout
    | AttemptDeleteAccount String
    | AttemptCreateTrip Date.Date Date.Date
    | AttemptDeleteTrip Trip
    | AttemptInviteUser Role
    | AttemptUpdateTrip TripPK Trip
      -- Inbound network
    | GotAccounts (WebData (List Account))
    | GotTrips (WebData (List Trip))
    | GotSignUp (Result Http.Error Session)
    | GotLogin (Result Http.Error Session)
    | GotLogout (Result Http.Error String)
    | GotDeleteAccount (Result Http.Error String)
    | GotCreateTrip (Result Http.Error ())
    | GotDeleteTrip (Result Http.Error ())
    | GotInviteUser (Result Http.Error ())
    | GotUpdateTrip (Result Http.Error ())


type Route
    = Login
    | UserHome
    | ManagerHome
    | AdminHome


type Role
    = User
    | Manager
    | Admin


type alias Account =
    { username : String
    , role : Role
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
    = Accounts
    | Trips


type LoginTab
    = LoginForm
    | SignUpForm


type alias Trip =
    { username : String
    , destination : String
    , startDate : Date.Date
    , endDate : Date.Date
    , comment : String
    }


type alias TripPK =
    { username : String
    , destination : String
    , startDate : Date.Date
    }


type alias Model =
    { route : Maybe Route
    , url : Url.Url
    , key : Nav.Key
    , session : Maybe Session
    , todaysDate : Maybe Date.Date
    , username : String
    , email : String
    , password : String
    , role : Maybe Role
    , accounts : WebData (List Account)
    , startDatePicker : DatePicker.DatePicker
    , endDatePicker : DatePicker.DatePicker
    , tripDestination : String
    , tripStartDate : Maybe Date.Date
    , tripEndDate : Maybe Date.Date
    , tripComment : String
    , trips : WebData (List Trip)
    , editingTrip : Maybe Trip
    , editTripDestination : String
    , editTripComment : String
    , adminTab : AdminTab
    , loginTab : LoginTab
    , inviteEmail : String
    , inviteRole : Maybe Role
    , inviteResponseStatus : WebData ()
    , updateTripStatus : WebData ()
    , loginError : Maybe Http.Error
    , logoutError : Maybe Http.Error
    , signUpError : Maybe Http.Error
    , deleteUserError : Maybe Http.Error
    , createTripError : Maybe Http.Error
    , deleteTripError : Maybe Http.Error
    , inviteUserError : Maybe Http.Error
    }


allErrors : Model -> List ( Maybe Http.Error, String )
allErrors model =
    [ ( model.loginError, "Error attempting to authenticate" )
    , ( model.logoutError, "Error attempting to log out" )
    , ( model.signUpError, "Error attempting to create your account" )
    , ( model.deleteUserError, "Error attempting to delete a user" )
    , ( model.createTripError, "Error attempting to create a trip" )
    , ( model.inviteUserError, "Error attempting to invite a user" )
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


encodeRole : Role -> JE.Value
encodeRole x =
    case x of
        User ->
            JE.string "user"

        Manager ->
            JE.string "manager"

        Admin ->
            JE.string "admin"


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

                x ->
                    JD.fail ("Invalid input: " ++ x)
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


updateTrip : TripPK -> Trip -> Cmd Msg
updateTrip tripKey trip =
    Utils.putWithCredentials
        { url = endpoint [ "trips" ] []
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "tripKey", encodeTripKey tripKey )
                    , ( "destination", JE.string trip.destination )
                    , ( "startDate", encodeDate trip.startDate )
                    , ( "endDate", encodeDate trip.endDate )
                    , ( "comment", JE.string trip.comment )
                    ]
                )
        , expect = Http.expectWhatever GotUpdateTrip
        }


inviteUser : { email : String, role : Role } -> Cmd Msg
inviteUser { email, role } =
    Utils.postWithCredentials
        { url = endpoint [ "invite" ] []
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "email", JE.string email )
                    , ( "role", encodeRole role )
                    ]
                )
        , expect = Http.expectWhatever GotInviteUser
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
        , expect = Http.expectWhatever GotCreateTrip
        }


deleteTrip :
    { username : String
    , destination : String
    , startDate : Date.Date
    }
    -> Cmd Msg
deleteTrip { username, destination, startDate } =
    Utils.deleteWithCredentials
        { url = endpoint [ "trips" ] []
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "username", JE.string username )
                    , ( "destination", JE.string destination )
                    , ( "startDate", encodeDate startDate )
                    ]
                )
        , expect = Http.expectWhatever GotDeleteTrip
        }


deleteAccount : String -> Cmd Msg
deleteAccount username =
    Utils.deleteWithCredentials
        { url = endpoint [ "accounts" ] [ UrlBuilder.string "username" username ]
        , body = Http.emptyBody
        , expect = Http.expectString GotDeleteAccount
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


encodeTripKey : TripPK -> JE.Value
encodeTripKey tripKey =
    JE.object
        [ ( "username", JE.string tripKey.username )
        , ( "destination", JE.string tripKey.destination )
        , ( "startDate", encodeDate tripKey.startDate )
        ]


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
                    (JD.map5
                        Trip
                        (JD.field "username" JD.string)
                        (JD.field "destination" JD.string)
                        (JD.field "startDate" decodeDate)
                        (JD.field "endDate" decodeDate)
                        (JD.field "comment" JD.string)
                    )
                )
        }


fetchAccounts : Cmd Msg
fetchAccounts =
    Utils.getWithCredentials
        { url = endpoint [ "accounts" ] []
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> GotAccounts)
                (JD.list
                    (JD.map2
                        Account
                        (JD.field "username" JD.string)
                        (JD.field "role" decodeRole)
                    )
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
      , todaysDate = Nothing
      , username = ""
      , email = ""
      , password = ""
      , role = Nothing
      , accounts = RemoteData.NotAsked
      , tripDestination = ""
      , tripStartDate = Nothing
      , tripEndDate = Nothing
      , tripComment = ""
      , trips = RemoteData.NotAsked
      , editingTrip = Nothing
      , editTripDestination = ""
      , editTripComment = ""
      , startDatePicker = startDatePicker
      , endDatePicker = endDatePicker
      , adminTab = Accounts
      , loginTab = LoginForm
      , inviteEmail = ""
      , inviteRole = Nothing
      , inviteResponseStatus = RemoteData.NotAsked
      , updateTripStatus = RemoteData.NotAsked
      , loginError = Nothing
      , logoutError = Nothing
      , signUpError = Nothing
      , deleteUserError = Nothing
      , createTripError = Nothing
      , deleteTripError = Nothing
      , inviteUserError = Nothing
      }
    , Cmd.batch
        [ Cmd.map UpdateTripStartDate startDatePickerCmd
        , Cmd.map UpdateTripEndDate endDatePickerCmd
        , Date.today |> Task.perform ReceiveTodaysDate
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
                [ { username = "mimi"
                  , destination = "Barcelona"
                  , startDate = Date.fromCalendarDate 2020 Time.Sep 25
                  , endDate = Date.fromCalendarDate 2020 Time.Oct 5
                  , comment = "Blah"
                  }
                , { username = "mimi"
                  , destination = "Paris"
                  , startDate = Date.fromCalendarDate 2021 Time.Jan 1
                  , endDate = Date.fromCalendarDate 2021 Time.Feb 1
                  , comment = "Bon voyage!"
                  }
                ]
      }
    , cmd
    )


managerHome : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
managerHome flags url key =
    let
        ( model, cmd ) =
            prod flags url key
    in
    ( { model
        | route = Just ManagerHome
        , session = Just { username = "bill", role = Manager }
      }
    , cmd
    )


adminHome : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
adminHome flags url key =
    let
        ( model, cmd ) =
            prod flags url key
    in
    ( { model
        | route = Just AdminHome
        , session = Just { username = "wpcarro", role = Admin }
      }
    , cmd
    )


port printPage : () -> Cmd msg


port googleSignIn : () -> Cmd msg


port googleSignOut : () -> Cmd msg


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

        UpdateEditTripDestination x ->
            ( { model | editTripDestination = x }, Cmd.none )

        UpdateEditTripComment x ->
            ( { model | editTripComment = x }, Cmd.none )

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

        PrintPage ->
            ( model, printPage () )

        GoogleSignIn ->
            ( model, googleSignIn () )

        GoogleSignOut ->
            ( model, googleSignOut () )

        UpdateInviteEmail x ->
            ( { model | inviteEmail = x }, Cmd.none )

        UpdateInviteRole mRole ->
            ( { model | inviteRole = mRole }, Cmd.none )

        ReceiveTodaysDate date ->
            ( { model | todaysDate = Just date }, Cmd.none )

        EditTrip trip ->
            ( { model
                | editingTrip = Just trip
                , editTripDestination = trip.destination
                , editTripComment = trip.comment
              }
            , Cmd.none
            )

        CancelEditTrip ->
            ( { model
                | editingTrip = Nothing
                , editTripDestination = ""
                , editTripComment = ""
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
                        , accounts = RemoteData.Loading
                      }
                    , fetchAccounts
                    )

                Just AdminHome ->
                    ( { model
                        | url = url
                        , route = route
                        , accounts = RemoteData.Loading
                        , trips = RemoteData.Loading
                      }
                    , Cmd.batch
                        [ fetchAccounts
                        , fetchTrips
                        ]
                    )

                _ ->
                    ( { model
                        | url = url
                        , route = route
                      }
                    , Cmd.none
                    )

        -- GET /accounts
        AttemptGetAccounts ->
            ( { model | accounts = RemoteData.Loading }, fetchAccounts )

        GotAccounts xs ->
            ( { model | accounts = xs }, Cmd.none )

        -- DELETE /accounts
        AttemptDeleteAccount username ->
            ( model, deleteAccount username )

        GotDeleteAccount result ->
            case result of
                Ok _ ->
                    ( model, fetchAccounts )

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

        GotCreateTrip result ->
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

        -- DELETE /trips
        AttemptDeleteTrip trip ->
            ( model
            , deleteTrip
                { username = trip.username
                , destination = trip.destination
                , startDate = trip.startDate
                }
            )

        GotDeleteTrip result ->
            case result of
                Ok _ ->
                    ( model, fetchTrips )

                Err e ->
                    ( { model | deleteTripError = Just e }
                    , sleepAndClearErrors
                    )

        AttemptInviteUser role ->
            ( { model | inviteResponseStatus = RemoteData.Loading }
            , inviteUser
                { email = model.inviteEmail
                , role = role
                }
            )

        GotInviteUser result ->
            case result of
                Ok _ ->
                    ( { model
                        | inviteEmail = ""
                        , inviteRole = Nothing
                        , inviteResponseStatus = RemoteData.Success ()
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model
                        | inviteUserError = Just e
                        , inviteResponseStatus = RemoteData.Failure e
                      }
                    , sleepAndClearErrors
                    )

        -- PATCH /trips
        AttemptUpdateTrip tripKey trip ->
            ( { model | updateTripStatus = RemoteData.Loading }
            , updateTrip tripKey trip
            )

        GotUpdateTrip result ->
            case result of
                Ok _ ->
                    ( { model | updateTripStatus = RemoteData.Success () }
                    , fetchTrips
                    )

                Err e ->
                    ( { model | updateTripStatus = RemoteData.Failure e }
                    , Cmd.none
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
        AttemptGetTrips ->
            ( { model | trips = RemoteData.Loading }, fetchTrips )

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
