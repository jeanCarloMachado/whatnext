module Loader exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required, src)


getLoadingHtml enabled =
    case enabled of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.8 ] ]
                [ Html.Styled.img [ src "/images/loader.gif", css [ maxWidth (px 100), maxHeight (px 100) ] ] []
                ]

        False ->
            emptyNode


emptyNode =
    text ""


disableLoading : Loading r -> Loading r
disableLoading model =
    { model | loading = False }


enableLoading : Loading r -> Loading r
enableLoading model =
    { model | loading = True }


type alias Loading r =
    { r
        | loading : Bool
    }
