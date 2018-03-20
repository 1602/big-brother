module View.JsonDiff exposing (view, classifyChange)

import Html exposing (..)
import Html.Attributes exposing (..)
import Component.JsonViewer as JsonViewer exposing (JsonViewer)
import Data.JsonDiff exposing (JsonDiff(..))


classifyChange : JsonDiff -> String
classifyChange delta =
    case delta of
        ValueAdded _ ->
            "delta__key delta__key--added"

        ValueModified _ _ ->
            "delta__key delta__key--modified"

        ValueDeleted _ ->
            "delta__key delta__key--deleted"

        _ ->
            "delta__key"


view : List String -> JsonViewer msg -> JsonDiff -> Html msg
view path jvr diff =
    let
        jsonViewer x =
            JsonViewer.view jvr path x
    in
        case diff of
            ObjectDiff props ->
                props
                    |> List.map
                        (\( key, diff ) ->
                            div []
                                [ span [ class (classifyChange diff) ] [ key ++ ":" |> text ]
                                , diff |> view (path ++ [ key ]) jvr
                                ]
                        )
                    |> div [ class "delta" ]

            ValueModified before after ->
                div []
                    [ div [ class "delta--deleted" ] [ before |> jsonViewer ]
                    , div [ class "delta--added" ] [ after |> jsonViewer ]
                    ]

            ValueAdded jv ->
                div [ class "delta--added" ]
                    [ jv |> jsonViewer
                    ]

            ArrayDelta ad ->
                ad |> jsonViewer

            JustValue jv ->
                jv |> jsonViewer

            ValueDeleted jv ->
                div [ class "delta--deleted" ]
                    [ jv |> jsonViewer
                    ]

            NoChanges ->
                text "Nothing changed"
