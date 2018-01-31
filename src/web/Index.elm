module Index exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Http exposing (..)
import Platform exposing (..)
import Json.Decode
import Json.Encode
import Loading
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Flags =
    { apiEndpoint : String }


type alias PageData =
    { subjects : List Subject
    , loading : Bool
    , toasterMsg : String
    , tiredMode : Bool
    , apiEndpoint : String
    }


type alias DoneData =
    { description : String
    , whatToDoNext : String
    }


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , open : Bool
    , doneForm : Bool
    , doneData : DoneData
    , whatToDoNext : String
    }


emptySubject =
    Subject "" 0 "" [] False False (DoneData "" "") ""


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }



-- Init


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] True "" False flags.apiEndpoint, getList flags.apiEndpoint False )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))
    | ExpandSubject Subject
    | GetDetail (Result Http.Error Subject)
    | DoneResult (Result Http.Error String)
    | SubmitDone Subject
    | ToggleTiredMode
    | StartDone Subject
    | CancelDone Subject
    | DoneChangeDescription Subject String
    | DoneChangeWhatToDoNext Subject String
    | None


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        NewList (Ok subjects) ->
            ( { model | subjects = subjects, loading = False }, Cmd.none )

        ExpandSubject subject ->
            case subject.history of
                [] ->
                    ( model, getDetail model.apiEndpoint { subject | open = True } )

                _ ->
                    ( (replaceSubjectFromList model { subject | open = not subject.open }), Cmd.none )

        GetDetail (Ok subject) ->
            let
                currentSubject =
                    subjectByName subject.name model.subjects

                newSubject =
                    { subject | open = currentSubject.doneForm, doneForm = currentSubject.doneForm }
            in
                ( (replaceSubjectFromList model newSubject), Cmd.none )

        GetDetail (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

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

        ToggleTiredMode ->
            let
                newModel =
                    { model | tiredMode = not model.tiredMode }
            in
                ( newModel, getList model.apiEndpoint newModel.tiredMode )

        None ->
            ( model, Cmd.none )


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
            Http.post url (Http.jsonBody body) decodeEmptyResult
    in
        Http.send DoneResult request


getDetail : String -> Subject -> Cmd Msg
getDetail endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/detail/" ++ subject.name

        request =
            Http.get url decodeSubject
    in
        Http.send GetDetail request


getList : String -> Bool -> Cmd Msg
getList endpoint tiredMode =
    let
        url =
            "http://" ++ endpoint ++ "/scheduler" ++ (tiredMode |> toUrlBool)

        request =
            Http.get url decodeSubjectList
    in
        Http.send NewList request


toUrlBool : Bool -> String
toUrlBool bool =
    case bool of
        True ->
            "?tiredMode=True"

        False ->
            ""



-- VIEW


view : PageData -> Html.Styled.Html Msg
view pageData =
    let
        loadingHtml =
            Loading.getHtml pageData.loading
    in
        div [ css [ top (px 0), left (px 0), margin (px 0), height (pct 100) ] ]
            [ loadingHtml
            , div [ css [ margin (pct 3) ] ]
                [ div []
                    [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                    , text "Tired mode"
                    ]
                , a [ href "index.html?env=development&page=log" ]
                    [ text "Log"
                    ]
                , getToasterHtml pageData
                , div
                    []
                    [ subjectsToHtml pageData.subjects
                    ]
                ]
            ]


subjectsToHtml list =
    let
        innerList =
            List.map subjectToHtml list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml subject =
    let
        historyHtml =
            subjectHistory subject

        nextStep =
            nextStepHtml subject

        doneForm =
            doneFormForSubject subject

        doneControlButtonsHtml =
            doneControlButtons subject
    in
        li [ onClick (ExpandSubject subject), subjectCss subject ]
            [ div []
                [ div [ css [ fontSize (Css.em 1.2) ] ]
                    [ text
                        (subject.name ++ ":  " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested))
                    , doneControlButtonsHtml
                    ]
                , div
                    [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed None)
                    ]
                    [ div []
                        [ doneForm
                        , nextStep
                        ]
                    , historyHtml
                    ]
                ]
            ]


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


inputCss =
    css [ display block, width (px 300), margin (px 5), padding (px 10) ]


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


nextStepHtml subject =
    case subject.open of
        True ->
            div [ css [ fontSize (Css.em 1.1) ] ]
                [ text <| "What to do next: " ++ subject.whatToDoNext
                ]

        False ->
            div [] []


subjectHistory subject =
    case subject.open of
        True ->
            div []
                [ text "History"
                , div [] (List.map studyEntryToHtml subject.history)
                ]

        False ->
            div [] []


subjectCss subject =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), selectedColor subject |> backgroundColor ]


selectedColor subject =
    case subject.open of
        True ->
            rgb 90 200 250

        _ ->
            hex "ffffff"


studyEntryToHtml studyEntry =
    div [ css [ padding (px 5) ] ]
        [ text (studyEntry.date)
        , span []
            [ text (": " ++ studyEntry.description)
            ]
        ]


getToasterHtml pageData =
    case String.length pageData.toasterMsg of
        0 ->
            div [] []

        _ ->
            div [ css [ borderStyle dashed, borderWidth (px 1), textAlign center ] ]
                [ text pageData.toasterMsg
                ]


replaceSubjectFromList model subject =
    let
        newList =
            (List.map (\x -> replaceSame subject x) model.subjects)
    in
        { model | subjects = newList }


replaceSame new orig =
    case orig.name == new.name of
        True ->
            new

        False ->
            orig


subjectByName subjectName subjectList =
    case List.filter (\x -> x.name == subjectName) subjectList of
        a :: _ ->
            a

        _ ->
            emptySubject


decodeSubjectList : Decoder (List Subject)
decodeSubjectList =
    Json.Decode.list decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodeStudyEntry) []
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded (DoneData "" "")
        |> Json.Decode.Pipeline.required "what_to_do_next" (Json.Decode.string)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.list decodeStudyEntry)


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


decodeEmptyResult =
    Json.Decode.succeed ""



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
