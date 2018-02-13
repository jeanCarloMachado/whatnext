module Log exposing (..)

import Css exposing (..)
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import View exposing (defaultColors, getLoadingHtml, studyEntryToHtml)
import Models exposing (StudyEntry, decodeStudyEntry)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias PageData =
    { history : List StudyEntry, toasterMsg : String, loading : Bool }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] "" True, getHistory flags.apiEndpoint )


type Msg
    = None
    | HistoryResult (Result Http.Error (List StudyEntry))


update msg pageData =
    case msg of
        HistoryResult (Ok historyList) ->
            ( { pageData | history = historyList, loading = False }, Cmd.none )

        HistoryResult (Err msg) ->
            ( { pageData | toasterMsg = toString msg }, Cmd.none )

        None ->
            ( pageData, Cmd.none )


getHistory endpoint =
    let
        url =
            "https://" ++ endpoint ++ "/log"

        request =
            Http.get url decodeHistory
    in
        Http.send HistoryResult request


decodeHistory =
    Json.Decode.list decodeStudyEntry


view pageData =
    let
        historyHtml =
            getHistoryHtml pageData

        loadingHtml =
            getLoadingHtml pageData.loading
    in
        div [ css [ color defaultColors.textNormal ] ]
            [ loadingHtml
            , div
                []
                [ a [ href "index.html" ]
                    [ text "Back"
                    ]
                ]
            , div []
                [ historyHtml
                ]
            ]


getHistoryHtml pageData =
    ul [ css [ listStyleType none ] ]
        (List.map studyEntryToHtml pageData.history)


subscriptions : PageData -> Sub Msg
subscriptions pageData =
    Sub.none
