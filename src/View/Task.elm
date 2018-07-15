module View.Task exposing (preview, details)

import Html exposing (..)
import Html.Attributes exposing (..)
import FeatherIcons
import View.App exposing (detailsBlock)
import View.Http
import View.Icons exposing (mediumIcon)
import Data.Event exposing (TaskReport(..))
import Strftime
import Date
import Json.Encode as Encode
import Json.Value as JsonValue exposing (JsonValue)
import View.Json


preview : TaskReport -> Html msg
preview task =
    case task of
        HttpRequest http ->
            div [ class "task-report http-request" ]
                [ View.Icons.send
                , span [ class ("method " ++ (http.request.method |> String.toLower)) ] [ http.request.method |> text ]
                , span [ class "url" ] [ http.request.url |> text ]
                , case http.response of
                    Just res ->
                        View.Http.statusCodeBadge res.status

                    Nothing ->
                        text ""
                ]

        CurrentTime x ->
            div [ class "task-report" ]
                [ FeatherIcons.clock |> mediumIcon
                , span [] [ Strftime.format "%B %d %Y, %H:%M:%S" (Date.fromTime x) |> text ]
                ]

        FailTask err ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsDown |> mediumIcon
                , case err of
                    Data.Event.ObjectWithMessage { message } ->
                        span [] [ message |> text ]

                    _ ->
                        text "Misc error"
                ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsUp |> mediumIcon
                  --, span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        GenericTask preview spec _ ->
            div [ class "task-report" ]
                [ case preview of
                    Just p ->
                        p |> text

                    Nothing ->
                        spec |> JsonValue.encode |> Encode.encode 0 |> text
                ]

        x ->
            x |> toString |> text


viewJsonValueExpandedRoot : JsonValue -> Html msg
viewJsonValueExpandedRoot val =
    Html.node "json-viewer"
        [ attribute "value" <| Encode.encode 0 <| JsonValue.encode val
        , attribute "expanded-nodes" "[[]]"
        ]
        []


details : Int -> TaskReport -> Html msg
details duration tr =
    case tr of
        HttpRequest http ->
            div [ class "http" ]
                [ detailsBlock "Request"
                    [ div [ class "task-report http-request" ]
                        [ span [ class ("method " ++ (http.request.method |> String.toLower)) ] [ http.request.method |> text ]
                        , span [ class "url" ] [ http.request.url |> text ]
                        ]
                    , h4 [] [ text "Headers" ]
                    , http.request.headers
                        |> List.map
                            (\( header, value ) ->
                                div []
                                    [ span [ class "http__header-name" ] [ text header ]
                                    , span [ class "http__header-value" ] [ value |> text ]
                                    ]
                            )
                        |> div []
                    , h4 [] [ text "Body" ]
                    , div []
                        [ http.request.data
                            |> Maybe.map (View.Json.view [])
                            |> Maybe.withDefault ("Ø" |> text)
                        ]
                    ]
                , detailsBlock "Response" <|
                    case http.response of
                        Just response ->
                            [ div []
                                [ View.Http.statusCodeBadge response.status
                                , span [] [ " " ++ response.status.text |> text ]
                                ]
                            , h4 [] [ text "Headers" ]
                            , response.headers
                                |> List.map
                                    (\( header, values ) ->
                                        div []
                                            [ span [ class "http__header-name" ] [ text header ]
                                            , span [ class "http__header-value" ] [ values |> String.join ", " |> text ]
                                            ]
                                    )
                                |> div []
                            , h4 [] [ text "Body" ]
                            , response.body |> Maybe.map (View.Json.view []) |> Maybe.withDefault (text "Ø")
                            ]

                        Nothing ->
                            case http.error of
                                Just error ->
                                    [ h4 [] [ text "Error" ]
                                    , div []
                                        [ error |> View.Json.view []
                                        ]
                                    ]

                                Nothing ->
                                    [ text "In progress, perhaps" ]
                , detailsBlock "Duration" <|
                    [ toString duration ++ "ms" |> text
                    ]
                ]

        CurrentTime x ->
            div [ class "task-report" ]
                [ FeatherIcons.clock |> mediumIcon
                , span [] [ Strftime.format "%B %d %Y, %-I:%M:%S" (Date.fromTime x) |> text ]
                ]

        FailTask err ->
            detailsBlock "Error" <|
                case err of
                    Data.Event.ObjectWithMessage { message, error } ->
                        [ h4 [] [ text message ]
                        , error |> View.Json.view []
                        ]

                    Data.Event.Unformatted error ->
                        [ error |> View.Json.view []
                        ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        GenericTask preview spec res ->
            div []
                [ detailsBlock "Spec" <|
                    [ spec |> viewJsonValueExpandedRoot
                    ]
                , case res of
                    Ok data ->
                        detailsBlock "Data" <|
                            [ data |> viewJsonValueExpandedRoot
                            ]

                    Err error ->
                        detailsBlock "Error" <|
                            [ error |> viewJsonValueExpandedRoot
                            ]
                , detailsBlock "Duration" <|
                    [ toString duration ++ "ms" |> text
                    ]
                ]

        UnknownTask x ->
            x |> toString |> text
