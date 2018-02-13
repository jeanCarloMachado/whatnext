module Log exposing (..)

import Css exposing (..)
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Colors exposing (defaultColors)


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


studyEntryToHtml studyEntry =
    li []
        [ p [ css [ color defaultColors.textHighlight ] ] [ text <| "Subject: " ++ studyEntry.subjectName ]
        , p [] [ text <| "Date: " ++ studyEntry.date ]
        , p [] [ text <| "  " ++ studyEntry.description ]
        ]


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


emptyNode =
    text ""


getLoadingHtml enabled =
    case enabled of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.9 ] ]
                [ text "Loading"
                ]

        False ->
            emptyNode
