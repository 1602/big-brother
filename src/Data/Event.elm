module Data.Event exposing (Event(..), TaskReport(..), decoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Data.JsonDiff exposing (JsonDiff)
import JsonValue exposing (JsonValue)
import Data.Http
import Time exposing (Time)


type alias Message =
    { name : String
    , payload : JsonValue
    }


type alias Id =
    String


type Event
    = StateUpdate Id JsonValue JsonValue JsonDiff Message
    | TaskEvent Id Int Bool TaskReport


type TaskReport
    = HttpRequest Data.Http.HttpClientTransaction
    | FailTask Value
    | SucceedTask Value
    | CurrentTime Time
    | UnknownTask Value


decoder : Decoder Event
decoder =
    Decode.oneOf [ stateUpdateDecoder, taskDecoder ]


stateUpdateDecoder : Decoder Event
stateUpdateDecoder =
    Decode.map5 StateUpdate
        (Decode.field "id" Decode.string)
        (Decode.field "cmd" JsonValue.decoder)
        (Decode.field "state" JsonValue.decoder)
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


taskReportDecoder : Decoder TaskReport
taskReportDecoder =
    Decode.oneOf
        [ Data.Http.decoder |> Decode.map HttpRequest
        , currentTimeTaskDecoder
        , failSucceedTaskDecoder
        , Decode.value |> Decode.map UnknownTask
        ]


failSucceedTaskDecoder : Decoder TaskReport
failSucceedTaskDecoder =
    Decode.at [ "spec", "task" ] Decode.string
        |> Decode.andThen
            (\task ->
                if task == "fail" then
                    Decode.map FailTask (Decode.at [ "spec", "error" ] Decode.value)
                else if task == "succeed" then
                    Decode.map SucceedTask (Decode.at [ "spec", "data" ] Decode.value |> Decode.maybe |> Decode.map (Maybe.withDefault Encode.null))
                else
                    Decode.fail "Doesn't look like fail task"
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
