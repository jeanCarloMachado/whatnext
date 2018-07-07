module Style exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required)
import Html.Styled.Events exposing (..)
import Dom.Scroll
import Css exposing (..)
import SDK exposing (Subject, PastAction, DoneData)
import Json.Decode


type alias Colors =
    { textHighlight : Css.Color
    , textNormal : Css.Color
    , selectedBackground : Css.Color
    , normalBackground : Css.Color
    , confirmButton : Css.Color
    , normalButton : Css.Color
    , warning : Css.Color
    , success : Css.Color
    , fail : Css.Color
    , barColor : Css.Color
    }


defaultColors =
    Colors
        (Css.hex "ff9500")
        (Css.hex "8e8e93")
        (Css.rgb 255 204 0)
        (Css.hex "e5e5e5")
        (Css.rgb 76 217 100)
        (Css.rgb 0 122 255)
        (Css.rgb 255 204 0)
        (Css.rgb 76 217 100)
        (Css.hex "ff2d55")
        (Css.rgb 0 122 255)


modalCss =
    css
        [ zIndex (Css.int 666)
        , justifyContent center
        , alignItems center
        , position fixed
        , displayFlex
        , top (px 0)
        , left (px 0)
        , width (pct 100)
        , height (pct 100)
        , backgroundColor <| rgba 255 255 255 1
        ]


emptyNode =
    text ""


buttonCss =
    [ minWidth (px 60)
    , margin (px 3)
    , minHeight (px 25)
    , textDecoration none
    , paddingTop (px 15)
    , paddingBottom (px 15)
    , paddingLeft (px 32)
    , paddingRight (px 32)
    , border (px 0)
    , textDecoration none
    , color <| Css.rgb 255 255 255
    , backgroundColor defaultColors.normalButton
    , textAlign center
    ]


textAreaCss =
    [ borderRadius (px 0)
    , borderStyle solid
    , borderColor (Css.hex "efeff4")
    , display block
    , width (px 300)
    , margin (px 1)
    , marginBottom (px 3)
    , padding (px 3)
    , height (px 150)
    ]

labelCss =
    []

inputCss =
    css
        [ borderRadius (px 0)
        , borderStyle solid
        , borderColor (Css.hex "efeff4")
        , display block
        , minWidth (px 300)
        , margin (px 1)
        , marginBottom (px 3)
        , padding (px 10)
        ]


selectCss =
    [ borderColor (Css.hex "efeff4")
    , borderRadius (px 0)
    , borderWidth (px 2)
    , color defaultColors.textNormal
    , width (px 300)
    , display block
    , backgroundColor (Css.hex "fff")
    , minHeight (px 35)
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


inlineIf test ifTrue ifFalse =
    if test then
        ifTrue
    else
        ifFalse


