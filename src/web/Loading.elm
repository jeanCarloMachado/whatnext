module Loading exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Css exposing (..)


emptyNode =
    text ""


getHtml enabled =
    case enabled of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.9 ] ]
                [ text "Loading"
                ]

        False ->
            emptyNode
