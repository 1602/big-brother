module View.JsonDiff exposing (view, classifyChange)

import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Component.JsonViewer as JsonViewer exposing (JsonViewer)
import Data.JsonDiff exposing (JsonDiff(..))
import Json.Encode as Encode exposing (string, list)
import Json.Value as JsonValue exposing (JsonValue)


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


viewJsonValue : List (List String) -> JsonValue -> Html msg
viewJsonValue expandedNodes val =
    Html.node "json-viewer"
        [ Html.Attributes.attribute "value" <| Encode.encode 0 <| JsonValue.encode val
        , Html.Attributes.attribute "expanded-nodes" <| Encode.encode 0 <| list <| List.map (List.map string >> list) <| expandedNodes
        ]
        []


view : List String -> JsonDiff -> Html msg
view path diff =
    let
        jsonViewer x =
            viewJsonValue [] x
    in
        case diff of
            ObjectDiff props ->
                props
                    |> List.map
                        (\( key, diff ) ->
                            div []
                                [ span [ class (classifyChange diff) ] [ key ++ ":" |> text ]
                                , diff |> view (path ++ [ key ])
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
