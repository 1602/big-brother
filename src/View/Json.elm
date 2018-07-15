module View.Json exposing (view)

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)
import Json.Value as JsonValue exposing (JsonValue)
import Json.Encode as Encode exposing (string, list, encode)


view : List (List String) -> JsonValue -> Html msg
view expandedNodes val =
    node "json-viewer"
        [ attribute "value" <| encode 0 <| JsonValue.encode val
        , attribute "expanded-nodes" <| encode 0 <| list <| List.map (List.map string >> list) <| expandedNodes
        ]
        []
