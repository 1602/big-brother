module Request.Application exposing (start, pause, resume, continue)

import HttpBuilder exposing (post, withExpect, toRequest, withJsonBody)
import Json.Value as JsonValue exposing (JsonValue)
import Http exposing (Request, Error)
import Json.Encode as Encode exposing (Value)
import Data.Event exposing (Message)


--import Json.Decode as Decode
--import Request.Helpers exposing (withListExpected, withJsonExpected)


addCheckpoint : Maybe ( JsonValue, String, Message ) -> List ( String, Value ) -> List ( String, Value )
addCheckpoint checkpoint body =
    case checkpoint of
        Just ( state, rayId, msg ) ->
            ( "state", JsonValue.encode state )
                :: ( "message", Encode.string msg.name )
                :: ( "payload", JsonValue.encode msg.payload )
                :: ( "rayId", Encode.string rayId )
                :: body

        _ ->
            body


start : Bool -> Maybe ( JsonValue, String, Message ) -> String -> Request ()
start paused checkpoint url =
    let
        body =
            [ ( "paused", Encode.bool paused )
            ]
                |> addCheckpoint checkpoint
                |> Encode.object
    in
        url
            ++ "/start"
            |> post
            |> withJsonBody body
            |> toRequest


pause : String -> Request ()
pause url =
    url
        ++ "/pause"
        |> post
        |> toRequest


resume : Maybe ( JsonValue, String, Message ) -> String -> Request ()
resume checkpoint url =
    let
        body =
            []
                |> addCheckpoint checkpoint
                |> Encode.object
    in
        url
            ++ "/resume"
            |> post
            |> withJsonBody body
            |> toRequest


continue : String -> Request ()
continue url =
    url
        ++ "/continue"
        |> post
        |> toRequest



{-
   create : String -> Credentials -> Request Client
   create clientName credentials =
       "/private/clients/"
           |> post
           |> withJsonExpected Client.decoder
           |> withJsonBody (Encode.object [ ( "name", Encode.string clientName ) ])
           |> toRequest
-}
