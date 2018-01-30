module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Http exposing (..)
import Platform exposing (..)
import Entities exposing (..)
import Decoders exposing (..)
import Json.Decode
import Json.Encode


type alias Flags =
    { apiEndpoint : String }


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
                    ( (Entities.replaceSubjectFromList model { subject | open = not subject.open }), Cmd.none )

        GetDetail (Ok subject) ->
            let
                currentSubject =
                    Entities.subjectByName subject.name model.subjects

                newSubject =
                    { subject | open = currentSubject.doneForm, doneForm = currentSubject.doneForm }
            in
                ( (Entities.replaceSubjectFromList model newSubject), Cmd.none )

        GetDetail (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        StartDone subject ->
            ( (Entities.replaceSubjectFromList model { subject | doneForm = True, open = True }), Cmd.none )

        CancelDone subject ->
            ( (Entities.replaceSubjectFromList model { subject | doneForm = False }), Cmd.none )

        DoneChangeDescription subject description ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | description = description }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( (Entities.replaceSubjectFromList model newSubject), Cmd.none )

        DoneChangeWhatToDoNext subject next ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | whatToDoNext = next }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( Entities.replaceSubjectFromList model newSubject, Cmd.none )

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


emptyNode =
    text ""


getLoadingHtml pageData =
    case pageData.loading of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.9 ] ]
                [ text "Loading"
                ]

        False ->
            emptyNode


view : PageData -> Html.Styled.Html Msg
view pageData =
    let
        loadingHtml =
            getLoadingHtml pageData
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



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
