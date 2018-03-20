module View.Http exposing (statusCodeBadge)

import Html exposing (Html, span, text)
import Html.Attributes exposing (classList)
import Data.Http exposing (Status)


statusCodeBadge : Status -> Html msg
statusCodeBadge { ok, code } =
    span
        [ classList
            [ ( "badge", True )
            , ( "badge--success", ok )
            , ( "badge--failure", not ok )
            ]
        ]
        [ code
            |> toString
            |> text
        ]
