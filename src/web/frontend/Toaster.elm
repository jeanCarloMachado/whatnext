module Toaster exposing (html)

import Html
import Html.Styled exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)


html message =
    case String.length message of
        0 ->
            div [] []

        _ ->
            div [ css [ borderStyle dashed, borderWidth (px 1), textAlign center ] ]
                [ text message
                ]
