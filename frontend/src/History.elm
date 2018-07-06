module History exposing (..)

import Css exposing (..)
import Navigation
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import SDK exposing(..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Loader
import Menu
import View exposing (defaultColors)
import Array exposing (Array)


type alias Flags =
    {
      apiEndpoint : String,
      authToken : String
    }



main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias State =
    { history : Array PastAction
    , toasterMsg : String
    , loading : Bool
    , sideMenu : Bool
    , apiEndpoint : String
    , authToken : String
    }


type alias PastAction =
    { date : String
    , description : String
    , subjectName : String
    }


init : Flags -> ( State, Cmd Msg )
init flags =
    let
      state = State Array.empty "" True False flags.apiEndpoint flags.authToken
    in
      ( state, Http.send HistoryResult <| SDK.getHistory state)


type Msg
    = None
    | HistoryResult (Result Http.Error (Array PastAction))
    | ToggleSideMenu
    | GoToScheduler



update : Msg -> State -> (State, Cmd Msg)
update msg state =
    case msg of
        HistoryResult (Ok historyList) ->
            ( { state | history = historyList, loading = False }, Cmd.none )

        HistoryResult (Err msg) ->
            ( { state | toasterMsg = toString msg }, Cmd.none )

        GoToScheduler ->
            ( state, Navigation.load "?page=scheduler" )

        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )



-- view

view : State -> Html Msg
view state =
    let
        historyHtml =
            getHistoryHtml state
    in
        div [ css [ color defaultColors.textNormal ] ]
            [ -- conditional loading modals
              Loader.getLoadingHtml state.loading
            , Menu.sideBarHtmlOptional state <|
                Menu.sideBarHtml ToggleSideMenu 
            , Menu.topBarHtml ToggleSideMenu []
            , div []
                [ h1
                    [ css
                        [ margin (px 20)
                        , fontSize <| Css.em 1.9
                        , textAlign center
                        ]
                    ]
                    [ text "Past" ]
                ]
            , div []
                [ historyHtml
                ]
            ]


getHistoryHtml : State -> Html Msg
getHistoryHtml state =
    ul [ css [ listStyleType none, width (pct 100) ] ]
        (Array.indexedMap (pastEntryToHtml <| Array.length state.history) state.history |> Array.toList)


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none

pastEntryToHtml : Int -> Int ->  PastAction -> Html Msg
pastEntryToHtml total indice pastEntry =
    let
        historyNumber =
            total - indice
    in
        li []
            [ div [ css [] ]
                [ div
                    [ css
                        [ backgroundColor <| Css.hex "fff"
                        , margin (px 30)
                        , padding (px 10)
                        , position relative
                        ]
                    ]
                    [
                     h2
                        [ css
                            [ color defaultColors.textHighlight
                            , fontSize <| Css.em 1.6
                            , display inline
                            ]
                        ]
                        [ text <| pastEntry.subjectName ]
                     ,span
                        [ css
                            [ fontSize (Css.em 0.5)
                            , marginLeft (px 15)
                            , position absolute
                            , top (px 5)
                            , right (px 5)
                            ]
                        ]
                        [ text <| (toString historyNumber) ]
                    , p
                        [ css
                            [ color defaultColors.textNormal
                            , fontSize (Css.em 0.8)
                            ]
                        ]
                        [ text pastEntry.date ]
                    , div [ css [ margin (px 20) ] ] [ text pastEntry.description ]
                    ]
                ]
            ]


