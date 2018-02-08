module Log exposing (..)

import Css exposing (..)
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Loading
import StudyEntry
import Global exposing (defaultColors)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias PageData =
    { history : List StudyEntry.Data, toasterMsg : String, loading : Bool }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] "" True, getHistory flags.apiEndpoint )


type Msg
    = None
    | HistoryResult (Result Http.Error (List StudyEntry.Data))


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
            "http://" ++ endpoint ++ "/log"

        request =
            Http.get url decodeHistory
    in
        Http.send HistoryResult request


decodeHistory =
    Json.Decode.list StudyEntry.decodeStudyEntry


view pageData =
    let
        historyHtml =
            getHistoryHtml pageData

        loadingHtml =
            Loading.getHtml pageData.loading
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
        (List.map StudyEntry.toHtml pageData.history)


subscriptions : PageData -> Sub Msg
subscriptions pageData =
    Sub.none
