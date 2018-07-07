module Toaster exposing (html)

import Html
import Html.Styled exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Style exposing (defaultColors)


html message =
    case String.length message of
        0 ->
            text ""

        _ ->
            div
                [ css
                    [ borderWidth (px 1)
                    , borderStyle solid
                    , borderColor defaultColors.textNormal
                    , color defaultColors.textNormal
                    , fontWeight bold
                    , padding (px 10)
                    , margin (px 10)
                    , textAlign center
                    ]
                ]
                [ text message
                ]
