module History exposing (..)

import Css exposing (..)
import Navigation
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Loader
import Menu
import View exposing (defaultColors)
import Array exposing (Array)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias PageData =
    { history : Array StudyEntry
    , toasterMsg : String
    , loading : Bool
    , sideMenu : Bool
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData Array.empty "" True False, getHistory flags.apiEndpoint )


type Msg
    = None
    | HistoryResult (Result Http.Error (Array StudyEntry))
    | ToggleSideMenu
    | GoToScheduler


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


getHistory endpoint =
    let
        url =
            "https://" ++ endpoint ++ "/log"

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeHistory)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send HistoryResult request


decodeHistory =
    Json.Decode.array decodeStudyEntry



-- view


view state =
    let
        historyHtml =
            getHistoryHtml state
    in
        div [ css [ color defaultColors.textNormal ] ]
            [ -- conditional loading modals
              Loader.getLoadingHtml state.loading
            , Menu.sideBarHtmlOptional state <|
                Menu.sideBarHtml ToggleSideMenu <|
                    a [ css <| List.append View.buttonCss [ marginTop (px 20) ], href "?page=scheduler" ]
                        [ text "Scheduler"
                        ]
            , Menu.topBarHtml ToggleSideMenu []
            , div []
                [ h1
                    [ css
                        [ margin (px 20)
                        , fontSize <| Css.em 1.9
                        , textAlign center
                        ]
                    ]
                    [ text "History" ]
                ]
            , div []
                [ historyHtml
                ]
            ]


getHistoryHtml state =
    ul [ css [ listStyleType none, width (pct 100) ] ]
        (Array.indexedMap (studyEntryToHtml <| Array.length state.history) state.history |> Array.toList)


subscriptions : PageData -> Sub Msg
subscriptions state =
    Sub.none


studyEntryToHtml total indice studyEntry =
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
                        [ text <| studyEntry.subjectName ]
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
                        [ text studyEntry.date ]
                    , div [ css [ margin (px 20) ] ] [ text studyEntry.description ]
                    ]
                ]
            ]


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
