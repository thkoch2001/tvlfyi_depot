module State exposing (..)


type Msg
    = DoNothing
    | SetView View


type View
    = Landing
    | Login


type alias Model =
    { isLoading : Bool
    , view : View
    }


{-| The initial state for the application.
-}
init : Model
init =
    { isLoading = False
    , view = Landing
    }


{-| Now that we have state, we need a function to change the state.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        SetView x ->
            ( { model
                | view = x
                , isLoading = True
              }
            , Cmd.none
            )
