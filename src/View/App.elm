module View.App exposing (layout)

import Html.Attributes exposing (..)
import Html exposing (..)


layout : { sidebar : List (Html msg), content : List (Html msg) } -> Html msg
layout { sidebar, content } =
    div [ class "app__container" ]
        [ div [ class "app__sidebar" ] sidebar
        , div [ class "app__content" ] content
        ]
