module Done exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Css exposing (..)
import Style exposing (defaultColors)
import Html.Styled.Events exposing (..)
import Json.Decode
import Html.Events.Extra exposing (targetValueIntParse)
import Menu
import Loader
import Toaster exposing (..)
import SDK
import Http
import Navigation
import SDK exposing (Subject)
import String


type alias Flags =
    { apiEndpoint : String
    , authToken : String
    , subjectName : String
    }


main =
    Html.programWithFlags
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( State, Cmd Msg )
init flags =
    let
        state =
            State "" False False "" flags.authToken flags.apiEndpoint
    in
        ( state, Cmd.none )


type alias State =
    { errorMessage : String
    , sideMenu : Bool
    , loading : Bool
    , toasterMsg : String
    , authToken : String
    , apiEndpoint : String
    }


type Msg
    = None
    | ToggleSideMenu


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )



view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml ToggleSideMenu

        --top menu
        , Menu.topBarHtml ToggleSideMenu
            [ button
                [ css <| List.append Style.buttonCss [ backgroundColor defaultColors.success ]
                ]
                [ text "Confirm" ]
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ Loader.getLoadingHtml state.loading
            , Toaster.html state.toasterMsg
            , div []
                [ content state
                ]
            ]
        ]


content state =
    div []
        [ div
            [ css
                [ displayFlex
                , justifyContent center
                , flexDirection column
                , alignItems center
                ]
            ]
            [ label [] [ text "What was done?" ]
            , textarea
                [ css Style.textAreaCss
                , placeholder "studied x y z"
                ]
                []
            , label [] [ text "What to do next" ]
            , textarea
                [ css Style.textAreaCss
                , placeholder "study x y z"
                ]
                []
            , label [ css Style.labelCss ] [ text "Subject name" ]
            , input
                [ Style.inputCss
                , type_ "text"
                , placeholder "Name"
                , Html.Styled.Attributes.required True
                ]
                []
            ]
        ]


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none


