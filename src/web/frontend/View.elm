module View exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required)
import Html.Styled.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Dom.Scroll
import Toaster exposing (..)
import Css exposing (..)
import Colors exposing (defaultColors)
import Subject exposing (Subject, StudyEntry, DoneData)
import Json.Decode


modalCss =
    css [ zIndex (Css.int 666), justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 1 ]


emptyNode =
    text ""


buttonCss =
    [ minWidth (px 60)
    , margin (px 3)
    , minHeight (px 30)
    , textDecoration none
    , paddingTop (px 15)
    , paddingBottom (px 15)
    , paddingLeft (px 32)
    , paddingRight (px 32)
    , border (px 0)
    , textDecoration none
    , color <| Css.rgb 255 255 255
    , backgroundColor defaultColors.normalButton
    ]


inputCss =
    css
        [ borderRadius (px 0)
        , borderStyle solid
        , display block
        , width (px 300)
        , margin (px 1)
        , marginBottom (px 3)
        , padding (px 10)
        ]


onClickStoppingPropagation msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed msg)


debugBorders css =
    let
        addWidth =
            borderWidth (px 1) :: css

        addStyle =
            borderStyle solid :: addWidth
    in
        addStyle


overrideBackgroundColor color css =
    List.append css [ backgroundColor color ]


optionFromTuple defaultValue ( value, label ) =
    if defaultValue == value then
        option [ Html.Styled.Attributes.value value, Html.Styled.Attributes.selected True ]
            [ text label ]
    else
        option [ Html.Styled.Attributes.value value ]
            [ text label ]


selectCss =
    [ display block
    , borderStyle solid
    , borderRightStyle none
    , width (px 300)
    , padding (Css.em 1.3)
    , margin (px 0)
    , backgroundColor (Css.hex "fff")
    ]
