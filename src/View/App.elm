module View.App exposing (layout, detailsBlock, error)

import Html.Attributes exposing (..)
import Html exposing (..)
import FeatherIcons
import View.Icons exposing (mediumIcon)


layout : { sidebar : List (Html msg), content : List (Html msg) } -> Html msg
layout { sidebar, content } =
    div [ class "app__container" ]
        [ div [ class "app__sidebar" ] sidebar
        , div [ class "app__content" ] content
        ]


detailsBlock : String -> List (Html msg) -> Html msg
detailsBlock header content =
    div [ class "details-block" ]
        [ h3 [ class "details-block__header" ] [ text header ]
        , section [ class "details-block__content" ] content
        ]


error : String -> Html msg
error message =
    span [ class "error" ]
        [ FeatherIcons.alertCircle |> mediumIcon
        , span [] [ text message ]
        ]
