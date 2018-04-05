module Component.JsonViewer exposing (JsonViewer, ExpandedNodes, view, toggle)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import JsonValue exposing (JsonValue)


type alias ExpandedNodes =
    List (List String)


type alias JsonViewer msg =
    { expandedNodes : ExpandedNodes
    , onToggle : List String -> msg
    }


toggle : List String -> ExpandedNodes -> ExpandedNodes
toggle path expandedNodes =
    if List.member path expandedNodes then
        expandedNodes |> List.filter ((/=) path)
    else
        path :: expandedNodes


view : JsonViewer msg -> List String -> JsonValue -> Html msg
view jvr path jv =
    case jv of
        JsonValue.BoolValue bv ->
            span [ class "json-value json-value--bool" ]
                [ text <|
                    if bv then
                        "true"
                    else
                        "false"
                ]

        JsonValue.NumericValue nv ->
            span [ class "json-value json-value--number" ] [ nv |> toString |> text ]

        JsonValue.StringValue sv ->
            span [ class "json-value json-value--string" ] [ sv |> toString |> text ]

        JsonValue.NullValue ->
            span [ class "json-value json-value--null" ] [ text "null" ]

        JsonValue.ObjectValue props ->
            if List.member path jvr.expandedNodes then
                props
                    |> List.map
                        (\( k, v ) ->
                            div [ class "json-value json-value__object-property" ]
                                [ span [ class "json-value json-value__key" ] [ text k ]
                                , v |> view jvr (path ++ [ k ])
                                ]
                        )
                    |> div [ class "json-value json-value--expandable" ]
            else
                props
                    |> List.take 5
                    |> List.map (\( k, _ ) -> k)
                    |> String.join ", "
                    |> (\s -> span [ class "json-value json-value--collapsed", onClick <| jvr.onToggle path ] [ "{ " ++ s ++ "... }" |> text ])

        JsonValue.ArrayValue items ->
            if List.member path jvr.expandedNodes then
                items
                    |> List.indexedMap
                        (\index v ->
                            div [ class "json-value json-value__array-item" ]
                                [ span [ class "json-value json-value__key" ] [ toString index |> text ]
                                , v |> view jvr (path ++ [ toString index ])
                                ]
                        )
                    |> div [ class "json-value json-value--expandable" ]
            else
                span
                    [ class "json-value json-value--collapsed"
                    , onClick <| jvr.onToggle path
                    ]
                    [ "[ " ++ (List.length items |> toString) ++ " items... ]" |> text
                    ]
