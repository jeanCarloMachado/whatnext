module Log exposing (..)

import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


type alias PageData =
    { history : List StudyEntry, toasterMsg : String }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] "", getHistory flags.apiEndpoint )


type Msg
    = None
    | HistoryResult (Result Http.Error (List StudyEntry))


update msg pageData =
    case msg of
        HistoryResult (Ok historyList) ->
            ( { pageData | history = historyList }, Cmd.none )

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
    Json.Decode.list decodeStudyEntry


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


view pageData =
    let
        historyHtml =
            getHistoryHtml pageData
    in
        div []
            [ div
                []
                [ a [ href "index.html?env=development" ]
                    [ text "Back"
                    ]
                ]
            , div []
                [ historyHtml
                ]
            ]


getHistoryHtml pageData =
    ul []
        (List.map getStudyEntryHtml pageData.history)


getStudyEntryHtml studyEntry =
    li []
        [ p [] [ text <| "Subject: " ++ studyEntry.subjectName ]
        , p [] [ text <| "Date: " ++ studyEntry.date ]
        , p [] [ text <| "Description: " ++ studyEntry.description ]
        ]


subscriptions : PageData -> Sub Msg
subscriptions pageData =
    Sub.none
