module Data.JsonDelta exposing (JsonDelta(..), decoder)

import Json.Decode as Decode exposing (Decoder)
import JsonValue exposing (JsonValue)


type JsonDelta
    = ObjectDiff (List ( String, JsonDelta ))
    | ValueAdded JsonValue
    | ValueModified JsonValue JsonValue
    | ValueDeleted JsonValue
    | NoChanges
    | ArrayDelta JsonValue
    | JustValue JsonValue


decoder : Decoder JsonDelta
decoder =
    Decode.oneOf
        [ Decode.field "_t" Decode.string
            |> Decode.andThen
                (\s ->
                    if s == "a" then
                        Decode.map ArrayDelta JsonValue.decoder
                    else
                        JsonValue.decoder |> Decode.map JustValue
                )
        , Decode.keyValuePairs (Decode.lazy (\_ -> decoder)) |> Decode.map ObjectDiff
        , Decode.list JsonValue.decoder
            |> Decode.andThen
                (\list ->
                    case list of
                        [] ->
                            NoChanges
                                |> Decode.succeed

                        [ newValue ] ->
                            ValueAdded newValue
                                |> Decode.succeed

                        [ oldValue, newValue ] ->
                            ValueModified oldValue newValue
                                |> Decode.succeed

                        [ oldValue, _, _ ] ->
                            ValueDeleted oldValue
                                |> Decode.succeed

                        _ ->
                            Decode.fail "Not a delta"
                )
        , JsonValue.decoder |> Decode.map JustValue
        ]
