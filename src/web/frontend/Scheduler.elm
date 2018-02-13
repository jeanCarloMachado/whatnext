module Scheduler exposing (..)

--view imports

import Html
import Html.Styled
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id)
import Html.Styled.Events exposing (..)
import Dom.Scroll
import Toaster exposing (..)
import Css exposing (..)
import Colors exposing (defaultColors)


-- json

import Json.Decode
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline


--rest

import Http exposing (..)
import Platform exposing (..)
import Array exposing (Array)
import Task


main =
    Html.programWithFlags { init = init, view = view >> Html.Styled.toUnstyled, update = update, subscriptions = subscriptions }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] Nothing True "" False flags.apiEndpoint, getListRequest flags.apiEndpoint False )



-- Model


type Msg
    = NewList (Result Http.Error (Array Subject))
    | ToggleTiredMode
    | None
    | MySubjectMsg SubjectMsg


type SubjectMsg
    = DoneResult (Result Http.Error String)
    | ExpandSubjectClick ( Int, Subject )
    | ClickDone Subject
    | CancelDone Subject
    | DoneChangeDescription Subject String
    | DoneChangeWhatToDoNext Subject String
    | Remove (Result Http.Error String)
    | RemoveClick Subject
    | GetDetail (Result Http.Error Subject)
    | SubmitDone Subject


type alias PageData =
    { subjects : List ( Int, Subject )
    , openedIndex : Maybe Int
    , loading : Bool
    , toasterMsg : String
    , tiredMode : Bool
    , apiEndpoint : String
    }


type alias Loading r =
    { r
        | loading : Bool
    }


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , doneForm : Bool
    , doneData : DoneData
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    }


type alias DoneData =
    { description : String
    , whatToDoNext : String
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


emptySubject : Subject
emptySubject =
    Subject "" 0 "" [] False (DoneData "" "") "" 0 0


type alias Flags =
    { apiEndpoint : String }



-- update


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Ok subjects) ->
            ( { model | subjects = Array.toIndexedList subjects, loading = False }, Cmd.none )

        MySubjectMsg a ->
            updateSubject a model

        NewList (Err msg) ->
            errorResult model msg

        None ->
            ( model, Cmd.none )

        ToggleTiredMode ->
            ( { model | tiredMode = not model.tiredMode }, getListRequest model.apiEndpoint <| not model.tiredMode )


updateSubject : SubjectMsg -> PageData -> ( PageData, Cmd Msg )
updateSubject msg model =
    case msg of
        ExpandSubjectClick ( indice, subject ) ->
            case model.openedIndex of
                Just indexVal ->
                    if indexVal == indice then
                        ( clickedSameIndex model, Cmd.none )
                    else
                        getDetailUpdateResult model indice subject

                Nothing ->
                    getDetailUpdateResult model indice subject

        GetDetail (Ok subject) ->
            let
                newModel =
                    (replaceSubjectFromList model subject)
            in
                ( model |> disableLoading, Task.attempt (always None) <| Dom.Scroll.toTop ("subject_" ++ toString (getOffsetOfSubject model.subjects subject)) )

        ClickDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = True }), Cmd.none )

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
            ( model |> startLoading, doneRequest model.apiEndpoint subject )

        DoneResult (Ok _) ->
            ( { model | loading = False }, getListRequest model.apiEndpoint model.tiredMode )

        RemoveClick subject ->
            ( model |> startLoading, removeRequest model.apiEndpoint subject )

        Remove (Ok _) ->
            ( model |> startLoading, getListRequest model.apiEndpoint model.tiredMode )

        Remove (Err msg) ->
            errorResult model msg

        DoneResult (Err msg) ->
            errorResult model msg

        GetDetail (Err msg) ->
            errorResult model msg


clickedSameIndex model =
    { model | openedIndex = Nothing } |> disableLoading


disableLoading : Loading r -> Loading r
disableLoading model =
    { model | loading = False }


startLoading model =
    { model | loading = True }


getOffsetOfSubject : List ( Int, Subject ) -> Subject -> Int
getOffsetOfSubject subjects subject =
    let
        filtered =
            List.filter (\x -> subject.name == (Tuple.second x).name) subjects
    in
        case filtered of
            [ a ] ->
                Tuple.first a

            _ ->
                0


getDetailUpdateResult : PageData -> Int -> Subject -> ( PageData, Cmd Msg )
getDetailUpdateResult model indice subject =
    ( { model | loading = True, openedIndex = Just indice }, getDetail model.apiEndpoint subject )


errorResult : PageData -> Error -> ( PageData, Cmd Msg )
errorResult model msg =
    ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )


replaceSubjectFromList : PageData -> Subject -> PageData
replaceSubjectFromList pageData subject =
    let
        newList =
            (List.map (\x -> replaceSame subject x) pageData.subjects)
    in
        { pageData | subjects = newList }


replaceSame : Subject -> ( Int, Subject ) -> ( Int, Subject )
replaceSame new ( indice, orig ) =
    case orig.name == new.name of
        True ->
            ( indice, new )

        False ->
            ( indice, orig )


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none



-- requests


getListRequest : String -> Bool -> Cmd Msg
getListRequest endpoint tiredMode =
    let
        url =
            "https://" ++ endpoint ++ "/scheduler" ++ (tiredMode |> toUrlBool)

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeSubjectList)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send NewList request


toUrlBool : Bool -> String
toUrlBool bool =
    case bool of
        True ->
            "?tiredMode=True"

        False ->
            ""


removeRequest : String -> Subject -> Cmd Msg
removeRequest endpoint subject =
    let
        url =
            "https://" ++ endpoint ++ "/rm/" ++ subject.name

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeEmptyResult)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send (MySubjectMsg << Remove) request


getDetail : String -> Subject -> Cmd Msg
getDetail endpoint subject =
    let
        url =
            "https://" ++ endpoint ++ "/detail/" ++ subject.name

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeSubject)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send (MySubjectMsg << GetDetail) request


doneRequest : String -> Subject -> Cmd Msg
doneRequest endpoint subject =
    let
        url =
            "https://" ++ endpoint ++ "/done/" ++ subject.name

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string subject.doneData.description )
                , ( "whatToDoNext", Json.Encode.string subject.doneData.whatToDoNext )
                ]

        request =
            Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = (Http.jsonBody body)
                , expect = (Http.expectJson decodeEmptyResult)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send (MySubjectMsg << DoneResult) request



-- decoders


decodeSubjectList : Decoder (Array Subject)
decodeSubjectList =
    Json.Decode.array decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodeStudyEntry) []
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded (DoneData "" "")
        |> Json.Decode.Pipeline.required "what_to_do_next" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "complexity" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "priority" (Json.Decode.int)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.array decodeStudyEntry)


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


decodeEmptyResult =
    Json.Decode.succeed ""



-- view


view : PageData -> Html.Styled.Html Msg
view pageData =
    let
        loadingHtml =
            getLoadingHtml pageData.loading
    in
        div [ css [ color defaultColors.textNormal, top (px 0), left (px 0), margin (px 0), height (pct 100) ] ]
            [ loadingHtml
            , div [ css [ margin (pct 3) ] ]
                [ div []
                    [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                    , text "Tired mode"
                    ]
                , a [ css [ padding (px 10) ], href "index.html?env=development&page=log" ]
                    [ text "Log"
                    ]
                , a [ css [ padding (px 10) ], href "index.html?page=add" ]
                    [ text "Add"
                    ]
                , Toaster.html pageData.toasterMsg
                , div
                    []
                    [ subjectsToHtml pageData.openedIndex pageData.subjects
                    ]
                ]
            ]


subjectsToHtml : Maybe Int -> List ( Int, Subject ) -> Html.Styled.Html Msg
subjectsToHtml openedIndex list =
    let
        innerList =
            List.map (subjectToHtml openedIndex) list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml : Maybe Int -> ( Int, Subject ) -> Html.Styled.Html Msg
subjectToHtml openedIndice ( indice, subject ) =
    li [ onClick ((MySubjectMsg << ExpandSubjectClick) ( indice, subject )), subjectCss openedIndice ( indice, subject ), id <| "subject_" ++ toString indice ]
        [ div []
            [ div [ css [ fontSize (Css.em 1.2) ] ]
                [ span [ css [ color defaultColors.textHighlight ] ] [ text subject.name ]
                , text
                    (" " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested))
                , (doneControlButtons subject)
                ]
            , (hiddenSubjectHtml openedIndice ( indice, subject ))
            ]
        ]


hiddenSubjectHtml : Maybe Int -> ( Int, Subject ) -> Html.Styled.Html Msg
hiddenSubjectHtml openedIndice ( indice, subject ) =
    case openedIndice of
        Just openedIndiceValue ->
            if openedIndiceValue == indice then
                div [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed None) ]
                    [ div []
                        [ (doneFormForSubject subject)
                        , div []
                            [ p []
                                [ text <| "Priority: " ++ (toString subject.priority)
                                ]
                            , p []
                                [ text <| "Complexity: " ++ (toString subject.complexity)
                                ]
                            ]
                        , div [ css [ fontSize (Css.em 1.1) ] ]
                            [ text <| "What to do next: " ++ subject.whatToDoNext
                            ]
                        ]
                    , div []
                        [ text "History"
                        , div [] (List.map studyEntryToHtml subject.history)
                        ]
                    , subjectButton "Remove" ((MySubjectMsg << RemoveClick) subject)
                    ]
            else
                emptyNode

        Nothing ->
            emptyNode


doneControlButtons : Subject -> Html.Styled.Html Msg
doneControlButtons subject =
    case subject.doneForm of
        True ->
            div [ css [ Css.float right ] ]
                [ subjectButton "Cancel" ((MySubjectMsg << CancelDone) subject)
                , subjectButton "Confirm" ((MySubjectMsg << SubmitDone) subject)
                ]

        False ->
            div [ css [ Css.float right ] ]
                [ subjectButton "Done" ((MySubjectMsg << ClickDone) subject)
                ]


subjectButton : String -> Msg -> Html.Styled.Html Msg
subjectButton textStr msg =
    button [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed msg) ]
        [ text textStr ]


inputCss : Attribute Msg
inputCss =
    css [ display block, width (px 300), margin (px 5), padding (px 10) ]


doneFormForSubject subject =
    case subject.doneForm of
        True ->
            div [ css [ paddingTop (px 10) ] ]
                [ input [ inputCss, type_ "text", placeholder "What was done?", onInput (MySubjectMsg << DoneChangeDescription subject) ] []
                , input [ inputCss, type_ "text", placeholder "What is to de done next?", onInput (MySubjectMsg << DoneChangeWhatToDoNext subject) ] []
                ]

        False ->
            div []
                []


subjectCss selectedIndex ( index, subject ) =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), backgroundColor (selectedColor selectedIndex ( index, subject )) ]


selectedColor selectedIndex ( index, subject ) =
    case selectedIndex of
        Just x ->
            if index == x then
                defaultColors.selectedBackground
            else
                defaultColors.normalBackground

        _ ->
            defaultColors.normalBackground


studyEntryToHtml studyEntry =
    li []
        [ p [ css [ color defaultColors.textHighlight ] ] [ text <| "Subject: " ++ studyEntry.subjectName ]
        , p [] [ text <| "Date: " ++ studyEntry.date ]
        , p [] [ text <| "  " ++ studyEntry.description ]
        ]


getLoadingHtml enabled =
    case enabled of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.9 ] ]
                [ text "Loading"
                ]

        False ->
            emptyNode


emptyNode =
    text ""
