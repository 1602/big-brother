module Data.Http exposing (Status, Request, Response, HttpClientTransaction, decoder)

import JsonValue exposing (JsonValue)
import Json.Decode as Decode exposing (Decoder)


type alias Status =
    { ok : Bool
    , code : Int
    , text : String
    }


type alias Response =
    { status : Status
    , headers : List ( String, List String )
    , error : Maybe JsonValue
    , body : Maybe JsonValue
    }


type alias Request =
    { url : String
    , method : String
    , headers : List ( String, String )
    , data : Maybe JsonValue
    }


type alias HttpClientTransaction =
    { request : Request
    , response : Maybe Response
    , error : Maybe JsonValue
    }


decoder : Decoder HttpClientTransaction
decoder =
    Decode.map3 HttpClientTransaction
        (Decode.field "spec" requestDecoder)
        (Decode.at [ "result", "data" ] responseDecoder |> Decode.maybe)
        (Decode.at [ "result", "error" ] JsonValue.decoder |> Decode.maybe)


statusDecoder : Decoder Status
statusDecoder =
    Decode.map3 Status
        (Decode.field "ok" Decode.bool)
        (Decode.field "code" Decode.int)
        (Decode.field "text" Decode.string)


responseDecoder : Decoder Response
responseDecoder =
    Decode.map4 Response
        (Decode.field "status" statusDecoder)
        (Decode.field "headers" (Decode.keyValuePairs (Decode.list Decode.string)))
        (Decode.field "error" JsonValue.decoder |> Decode.maybe)
        (Decode.field "body" JsonValue.decoder |> Decode.maybe)


requestDecoder : Decoder Request
requestDecoder =
    Decode.map4 Request
        (Decode.field "url" Decode.string)
        (Decode.field "method" Decode.string)
        (Decode.field "headers" (Decode.keyValuePairs Decode.string))
        (Decode.field "data" JsonValue.decoder |> Decode.maybe)
