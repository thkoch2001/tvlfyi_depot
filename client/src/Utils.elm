module Utils exposing (..)

import DateFormat
import Http
import Time
import Shared


explainHttpError : Http.Error -> String
explainHttpError e =
    case e of
        Http.BadUrl _ ->
            "Bad URL: you may have supplied an improperly formatted URL"

        Http.Timeout ->
            "Timeout: the resource you requested did not arrive within the interval of time that you claimed it should"

        Http.BadStatus s ->
            "Bad Status: the server returned a bad status code: " ++ String.fromInt s

        Http.BadBody b ->
            "Bad Body: our application had trouble decoding the body of the response from the server: " ++ b

        Http.NetworkError ->
            "Network Error: something went awry in the network stack. I recommend checking the server logs if you can."


getWithCredentials :
    { url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
getWithCredentials { url, expect } =
    Http.riskyRequest
        { url = url
        , headers = [ Http.header "Origin" Shared.clientOrigin ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , expect = expect
        }


postWithCredentials :
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
postWithCredentials { url, body, expect } =
    Http.riskyRequest
        { url = url
        , headers = [ Http.header "Origin" Shared.clientOrigin ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body = body
        , expect = expect
        }


deleteWithCredentials :
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
deleteWithCredentials { url, body, expect } =
    Http.riskyRequest
        { url = url
        , headers = [ Http.header "Origin" Shared.clientOrigin ]
        , method = "DELETE"
        , timeout = Nothing
        , tracker = Nothing
        , body = body
        , expect = expect
        }

putWithCredentials :
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
putWithCredentials { url, body, expect } =
    Http.riskyRequest
        { url = url
        , headers = [ Http.header "Origin" Shared.clientOrigin ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , body = body
        , expect = expect
        }



formatTime : Time.Posix -> String
formatTime ts =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]
        Time.utc
        ts
