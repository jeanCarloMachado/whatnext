module Menu exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Html.Styled.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Dom.Scroll
import Toaster exposing (..)
import Css exposing (..)
import Style exposing (defaultColors)
import DOM


type alias MenuState r =
    { r
        | sideMenu : Bool
    }


toggle menuState =
    { menuState | sideMenu = not menuState.sideMenu }


topBarHtml toggleMenuEvent elements =
    div
        [ css
            [ backgroundColor defaultColors.barColor
            , displayFlex
            , justifyContent spaceBetween
            , flexDirection row
            , minHeight (px 50)
            ]
        ]
        [ img
            [ css
                [ paddingLeft (px 3)
                , maxHeight (px 55)
                ]
            , src "images/expandMenu.png"
            , onClick toggleMenuEvent
            ]
            []
        , div [ css [ displayFlex, justifyContent flexEnd, alignItems center ] ]
            elements
        ]


sideBarHtmlOptional state configuredSidebar =
    Style.inlineIf (state.sideMenu) (configuredSidebar) Style.emptyNode

dropdownMenuItemCss = [
                zIndex (Css.int 333),
                marginTop (px 20)
    ]

sideBarHtml toggleMenuEvent =
    div
        [ css
            [ width (px 250)
            , position absolute
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , flexDirection column
                ]
            , onClick toggleMenuEvent
            ]
            [
                button [ css <| List.append Style.buttonCss [ textAlign left ] ]
                [ text "Close menu" ]

                ,    a [ css  <| List.append Style.buttonCss dropdownMenuItemCss, href "?page=scheduler" ]
                        [ text "Next Steps"
                        ]
                , a [ css <| List.append Style.buttonCss dropdownMenuItemCss, href "?page=log" ]
                    [ text "Past Actions"
                    ]
            , a
                [ css <| Style.overrideBackgroundColor defaultColors.fail <|
                  List.append Style.buttonCss dropdownMenuItemCss
                , href "/"
                ]
                [ text "Quit" ]
            ]
        ]
