module Log exposing (..)

import Css exposing (..)
import Navigation
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Colors exposing (defaultColors)
import Loader
import View


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias PageData =
    { history : List StudyEntry, toasterMsg : String, loading : Bool }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] "" True, getHistory flags.apiEndpoint )


type Msg
    = None
    | HistoryResult (Result Http.Error (List StudyEntry))
    | GoToScheduler


update msg pageData =
    case msg of
        HistoryResult (Ok historyList) ->
            ( { pageData | history = historyList, loading = False }, Cmd.none )

        HistoryResult (Err msg) ->
            ( { pageData | toasterMsg = toString msg }, Cmd.none )

        GoToScheduler ->
            ( pageData, Navigation.load "?page=scheduler" )

        None ->
            ( pageData, Cmd.none )


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
    Json.Decode.list decodeStudyEntry



-- view


view pageData =
    let
        historyHtml =
            getHistoryHtml pageData
    in
        div [ css [ color defaultColors.textNormal ] ]
            [ -- conditional loading modals
              Loader.getLoadingHtml pageData.loading
            , div
                [ css [ margin (px 10), marginTop (px 0) ] ]
                [ button [ css View.buttonCss, onClick GoToScheduler ]
                    [ text "Go Back"
                    ]
                ]
            , div []
                [ h1 [ css [ margin (px 20), fontSize <| Css.em 1.9, textAlign center ] ] [ text "History" ]
                ]
            , div []
                [ historyHtml
                ]
            ]


getHistoryHtml pageData =
    ul [ css [ listStyleType none, width (pct 100) ] ]
        (List.map studyEntryToHtml pageData.history)


subscriptions : PageData -> Sub Msg
subscriptions pageData =
    Sub.none


studyEntryToHtml studyEntry =
    li []
        [ div [ css [] ]
            [ div [ css [ backgroundColor <| Css.hex "fff", margin (px 30), padding (px 10) ] ]
                [ h2 [ css [ color defaultColors.textHighlight, fontSize <| Css.em 1.6 ] ] [ text <| studyEntry.subjectName ]
                , p [ css [ color defaultColors.textUninportant ] ] [ text studyEntry.date ]
                , div [ css [ margin (px 20) ] ] [ text studyEntry.description ]
                ]
            ]
        ]


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
