module Views.Http exposing (statusCodeBadge)

import Html exposing (Html, span, text)
import Html.Attributes exposing (classList)


statusCodeBadge : { ok : Bool, code : Int, text : String } -> Html msg
statusCodeBadge status =
    span
        [ classList
            [ ( "badge", True )
            , ( "badge--success", status.ok )
            , ( "badge--failure", not status.ok )
            ]
        ]
        [ status.code
            |> toString
            |> text
        ]
