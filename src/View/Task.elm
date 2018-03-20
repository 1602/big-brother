module View.Task exposing (preview, details)

import Html exposing (..)
import Html.Attributes exposing (..)
import FeatherIcons
import View.App exposing (detailsBlock)
import View.Http
import View.Icons exposing (mediumIcon)
import Data.Event exposing (TaskReport(..))
import Strftime
import Component.JsonViewer exposing (JsonViewer)
import Date
import Json.Encode as Encode
import JsonValue exposing (JsonValue)


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
                        span [ class "status-code" ] [ res.status.code |> toString |> text ]

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
                , span [] [ err |> toString |> text ]
                ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsUp |> mediumIcon
                  --, span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        x ->
            x |> toString |> text


details : Int -> TaskReport -> Component.JsonViewer.JsonViewer msg -> Html msg
details duration tr jsonViewer =
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
                            |> Maybe.map (viewJsonValue jsonViewer [ "requestBody" ])
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
                            , response.body |> Maybe.map (viewJsonValue jsonViewer [ "responseBody" ]) |> Maybe.withDefault (text "Ø")
                            ]

                        Nothing ->
                            case http.error of
                                Just error ->
                                    [ h4 [] [ text "Error" ]
                                    , div []
                                        [ error |> viewJsonValue jsonViewer []
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
            div [ class "task-report" ]
                [ FeatherIcons.thumbsDown |> mediumIcon
                , span [] [ err |> toString |> text ]
                ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        UnknownTask x ->
            x |> toString |> text


viewJsonValue : JsonViewer msg -> List String -> JsonValue -> Html msg
viewJsonValue jsonViewer path =
    Component.JsonViewer.view jsonViewer path
