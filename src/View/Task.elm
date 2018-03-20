module View.Task exposing (preview)

import Html exposing (..)
import Html.Attributes exposing (..)
import FeatherIcons
import View.Icons exposing (mediumIcon)
import Data.Event exposing (TaskReport(..))
import Strftime
import Date


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
