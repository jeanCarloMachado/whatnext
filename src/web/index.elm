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


main =
    Html.program { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }



-- Init


init : ( PageData, Cmd Msg )
init =
    ( PageData [] True "", getList )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))
    | ExpandSubject Subject
    | GetDetail (Result Http.Error Subject)
    | DoneResult (Result Http.Error String)
    | SubmitDone Subject
    | StartDone Subject
    | CancelDone Subject
    | None
    | DoneChangeDescription Subject String
    | DoneChangeWhatToDoNext Subject String


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( PageData [] False (toString msg), Cmd.none )

        NewList (Ok subjects) ->
            ( PageData subjects False "", Cmd.none )

        ExpandSubject subject ->
            case subject.history of
                [] ->
                    ( model, getDetail { subject | open = True } )

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
            ( model, doneRequest subject )

        DoneResult (Ok _) ->
            ( model, getList )

        DoneResult (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        None ->
            ( model, Cmd.none )


doneRequest : Subject -> Cmd Msg
doneRequest subject =
    let
        url =
            "http://whatnext:5000/done/" ++ subject.name

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string subject.doneData.description )
                , ( "whatToDoNext", Json.Encode.string subject.doneData.whatToDoNext )
                ]

        request =
            Http.post url (Http.jsonBody body) decodeEmptyResult
    in
        Http.send DoneResult request


getDetail : Subject -> Cmd Msg
getDetail subject =
    let
        url =
            "http://whatnext:5000/detail/" ++ subject.name

        request =
            Http.get url decodeSubject
    in
        Http.send GetDetail request


getList : Cmd Msg
getList =
    let
        url =
            "http://whatnext:5000"

        request =
            Http.get url decodeSubjectList
    in
        Http.send NewList request



-- VIEW


view : PageData -> Html.Styled.Html Msg
view pageData =
    div [ css [ position relative, top (px 0), left (px 0), margin (px 0), height (pct 100) ] ]
        [ div [ css [ margin (pct 3) ] ]
            [ getToasterHtml pageData
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
                [ text
                    (subject.name ++ ":  " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested |> toString))
                , doneControlButtonsHtml
                , div
                    [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed None)
                    ]
                    [ doneForm
                    , div []
                        (nextStep
                            :: (historyHtml)
                        )
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


doneFormForSubject subject =
    case subject.doneForm of
        True ->
            div [ css [ paddingTop (px 10) ] ]
                [ input [ type_ "text", placeholder "Done", onInput (DoneChangeDescription subject) ] []
                , input [ type_ "text", placeholder "Next Action", onInput (DoneChangeWhatToDoNext subject) ] []
                ]

        False ->
            div []
                []


nextStepHtml subject =
    case subject.open of
        True ->
            div []
                [ text <| "What to do next: " ++ subject.whatToDoNext
                ]

        False ->
            div [] []


subjectHistory subject =
    case subject.open of
        True ->
            List.map studyEntryToHtml subject.history

        False ->
            []


subjectCss subject =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), selectedColor subject |> backgroundColor ]


selectedColor subject =
    case subject.open of
        True ->
            hex "add8e6"

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
