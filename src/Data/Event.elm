module Data.Event exposing (Event(..), TaskReport(..), ErrorObject(..), Message, decoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Data.JsonDiff exposing (JsonDiff)
import Json.Value as JsonValue exposing (JsonValue)
import Data.Http
import Time exposing (Time)


type alias Message =
    { name : String
    , payload : JsonValue
    }


type alias Id =
    String


type Event
    = StateUpdate Id JsonValue JsonValue JsonValue JsonDiff Message
    | TaskEvent Id Int Bool TaskReport


type ErrorObject
    = ObjectWithMessage { message : String, error : JsonValue }
    | Unformatted JsonValue


type TaskReport
    = HttpRequest Data.Http.HttpClientTransaction
    | FailTask ErrorObject
    | SucceedTask Value
    | CurrentTime Time
    | GenericTask (Maybe String) JsonValue (Result JsonValue JsonValue)
    | UnknownTask Value



{-
   TODO:

   Add generic event with template for preview and details. Register generic event types on connection.
   Backend should have ability to describe templates for new custom events. Consider adding directives for filters / badges / titles / subtitles.

-}


decoder : Decoder Event
decoder =
    Decode.oneOf [ stateUpdateDecoder, taskDecoder ]


stateUpdateDecoder : Decoder Event
stateUpdateDecoder =
    Decode.map6 StateUpdate
        (Decode.field "id" Decode.string)
        (Decode.field "cmd" JsonValue.decoder)
        (Decode.field "state" JsonValue.decoder)
        (Decode.field "prevState" JsonValue.decoder)
        (Decode.field "diff" Data.JsonDiff.decoder)
        (Decode.field "msg" messageDecoder)


messageDecoder : Decoder Message
messageDecoder =
    Decode.map2 Message
        (Decode.field "name" Decode.string)
        (Decode.field "payload" JsonValue.decoder)


taskDecoder : Decoder Event
taskDecoder =
    Decode.map4 TaskEvent
        (Decode.field "id" Decode.string)
        (Decode.field "duration" Decode.int)
        (Decode.at [ "result", "result" ] (Decode.string |> Decode.map ((==) "success")))
        taskReportDecoder


genericTaskDecoder : Decoder TaskReport
genericTaskDecoder =
    Decode.map3 GenericTask
        (Decode.at [ "spec", "preview" ] Decode.string |> Decode.maybe)
        (Decode.field "spec" JsonValue.decoder)
        (Decode.field "result"
            (Decode.oneOf
                [ Decode.field "data" JsonValue.decoder |> Decode.map Ok
                , Decode.field "error" JsonValue.decoder |> Decode.map Err
                ]
            )
        )


taskReportDecoder : Decoder TaskReport
taskReportDecoder =
    Decode.oneOf
        [ Data.Http.decoder |> Decode.map HttpRequest
        , currentTimeTaskDecoder
        , failSucceedTaskDecoder
        , genericTaskDecoder
        , Decode.value |> Decode.map UnknownTask
        ]


failSucceedTaskDecoder : Decoder TaskReport
failSucceedTaskDecoder =
    Decode.at [ "spec", "task" ] Decode.string
        |> Decode.andThen
            (\task ->
                if task == "fail" then
                    Decode.map FailTask (Decode.at [ "spec", "error" ] errorObjectDecoder)
                else if task == "succeed" then
                    Decode.map SucceedTask (Decode.at [ "spec", "data" ] Decode.value |> Decode.maybe |> Decode.map (Maybe.withDefault Encode.null))
                else
                    Decode.fail "Doesn't look like fail task"
            )


errorObjectDecoder : Decoder ErrorObject
errorObjectDecoder =
    Decode.oneOf
        [ errorWithMessageDecoder
        , JsonValue.decoder |> Decode.map Unformatted
        ]


errorWithMessageDecoder : Decoder ErrorObject
errorWithMessageDecoder =
    Decode.field "message" Decode.string
        |> Decode.andThen
            (\message ->
                JsonValue.decoder |> Decode.map (\jv -> ObjectWithMessage { message = message, error = jv })
            )


currentTimeTaskDecoder : Decoder TaskReport
currentTimeTaskDecoder =
    Decode.map CurrentTime
        (Decode.field "spec" Decode.string
            |> Decode.andThen
                (\s ->
                    if s == "time" then
                        Decode.at [ "result", "data" ] Decode.float
                    else
                        Decode.fail "Not a current time task"
                )
        )
