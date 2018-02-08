module Done exposing (..)

import Json.Encode
import Html
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Subject exposing (..)
import Http exposing (..)
import Json.Decode
import Global exposing (inputCss)


type Msg
    = StartDone Subject
    | CancelDone Subject
    | DoneChangeDescription Subject String
    | DoneChangeWhatToDoNext Subject String
    | DoneResult (Result Http.Error String)
    | SubmitDone Subject


type alias PageData =
    { apiEndpoint : String
    }


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        StartDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = True, open = True }), Cmd.none )

        CancelDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = False }), Cmd.none )

        DoneChangeDescription subject description ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | description = description }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( (replaceSubjectFromList model newSubject), Cmd.none )

        DoneChangeWhatToDoNext subject next ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | whatToDoNext = next }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( replaceSubjectFromList model newSubject, Cmd.none )

        SubmitDone subject ->
            ( { model | loading = True }, doneRequest model.apiEndpoint subject )

        DoneResult (Ok _) ->
            ( { model | loading = False }, getList model.apiEndpoint model.tiredMode )

        DoneResult (Err msg) ->
            ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )


doneRequest : String -> Subject -> Cmd Msg
doneRequest endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/done/" ++ subject.name

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string subject.doneData.description )
                , ( "whatToDoNext", Json.Encode.string subject.doneData.whatToDoNext )
                ]

        request =
            Http.post url (Http.jsonBody body) Global.decodeEmptyResult
    in
        Http.send DoneResult request


doneFormForSubject subject =
    case subject.doneForm of
        True ->
            div [ css [ paddingTop (px 10) ] ]
                [ input [ inputCss, type_ "text", placeholder "What was done?", onInput (DoneChangeDescription subject) ] []
                , input [ inputCss, type_ "text", placeholder "What is to de done next?", onInput (DoneChangeWhatToDoNext subject) ] []
                ]

        False ->
            div []
                []


doneControlButtons subject =
    case subject.doneForm of
        True ->
            div [ css [ Css.float right ] ]
                [ button [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed (CancelDone subject)) ]
                    [ text "Cancel" ]
                , button
                    [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed (SubmitDone subject)) ]
                    [ text "Confirm" ]
                ]

        False ->
            div [ css [ Css.float right ] ]
                [ button [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed (StartDone subject)) ] [ text "Done" ]
                ]
