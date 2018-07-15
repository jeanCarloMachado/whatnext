module Menu exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Style exposing (defaultColors)


type alias MenuState r =
    { r
        | sideMenu : Bool
    }


toggle : MenuState r -> MenuState r
toggle menuState =
    { menuState | sideMenu = not menuState.sideMenu }


topBarHtml toggleMenuEvent title elements =
    div
        [ css
            [ backgroundColor defaultColors.barColor
            , displayFlex
            , justifyContent spaceBetween
            , flexDirection row
            , minHeight (px 50)
            , boxShadow5 (px 0) (px 4) (px 8) (px 1) (Css.rgba 0 0 0 0.14)
            ]
        ]
        [
          div [
            css
              [
               displayFlex
              , justifyContent spaceBetween
              , flexDirection row
              , minHeight (px 50)
              , alignItems center
            ]
            ]
          [
              img
                [ css
                    [ paddingLeft (px 3)
                    , maxHeight (px 55)
                    ]
                , src "images/expandMenu.png"
                , onClick toggleMenuEvent
                ]
                []
            , h1 [
              css [
                color defaultColors.invertedHighlight
                , fontSize (Css.em 1.7)
                ]
            ] [ text title ]
        ]
        , div [ css [ displayFlex, justifyContent flexEnd, alignItems center ] ]
            elements
        ]


sideBarHtmlOptional state configuredSidebar =
    Style.inlineIf (state.sideMenu) (configuredSidebar) Style.emptyNode


sideBarHtml : Html msg
sideBarHtml =
    div
        [ css
            [ width (px 200)
            , position absolute
            , top (px 50)
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , flexDirection column
                ]
            ]
            [ a [ css <| List.append Style.buttonCss dropdownMenuItemCss, href "?page=log" ]
                [ text "History"
                ]
            , a
                [ css <|
                    Style.overrideBackgroundColor defaultColors.fail <|
                        List.append Style.buttonCss dropdownMenuItemCss
                , href "/"
                ]
                [ text "Quit" ]
            ]
        ]


dropdownMenuItemCss =
    [ zIndex (Css.int 333)
    , marginTop (px 20)
    ]


