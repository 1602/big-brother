module View.Icons exposing (mediumIcon, send)

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import FeatherIcons exposing (Icon)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "18"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "18"
        ]


send : Html msg
send =
    svgFeatherIcon "send"
        [ Svg.line [ x1 "22", y1 "2", x2 "11", y2 "13" ] []
        , Svg.polygon [ points "22 2 15 22 11 13 2 9 22 2" ] []
        ]


mediumIcon : Icon -> Html msg
mediumIcon ib =
    ib |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
