module Scheduler exposing (..)

--view imports

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_, id, class, value, required)
import Html.Styled.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
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
import Loader
import Subject exposing (Subject, StudyEntry, DoneData)


main =
    Html.programWithFlags { init = init, view = view >> Html.Styled.toUnstyled, update = update, subscriptions = subscriptions }


init : Flags -> ( State, Cmd Msg )
init flags =
    ( State [] True "" False flags.apiEndpoint "" "" "" False 50 50 "" "" "", getListRequest flags.apiEndpoint False )


type alias State =
    { subjects : List ( Int, Subject )
    , loading : Bool
    , toasterMsg : String
    , tiredMode : Bool
    , apiEndpoint : String
    , doneSubjectName : String
    , doneDescription : String
    , doneWhatToDoNext : String
    , addSubjectModal : Bool
    , newComplexity : Int
    , newPriority : Int
    , newSubjectName : String
    , newWhatToDoNext : String
    , openedSubjectName : String
    }



-- Model


type Msg
    = NewList (Result Http.Error (Array Subject))
    | ToggleTiredMode
    | NoAction
    | MySubjectMsg SubjectMsg


type SubjectMsg
    = ExpandSubjectClick ( Int, Subject )
    | Remove (Result Http.Error String)
    | RemoveClick Subject
    | EditClick Subject
    | GetDetail (Result Http.Error Subject)
    | MyDoneMsg DoneMsg
    | OpenAddSubjectModal
    | CancelAddSubjectModal
    | ChangeNewSubjectName String
    | ChangeNewWhatToDoNext String
    | ChangeNewPriority Int
    | ChangeNewComplexity Int
    | SubmitNewSubject
    | NewSubjectResult (Result Http.Error String)


type DoneMsg
    = ClickDone Subject
    | DoneResult (Result Http.Error String)
    | DoneChangeDescription String
    | DoneChangeWhatToDoNext String
    | SubmitDone
    | CancelDone


type alias Flags =
    { apiEndpoint : String }



-- update


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        MySubjectMsg a ->
            updateSubject a model

        NewList (Ok subjects) ->
            ( { model | subjects = Array.toIndexedList subjects } |> Loader.disableLoading, Cmd.none )

        NewList (Err msg) ->
            errorResult model msg

        NoAction ->
            ( model, Cmd.none )

        ToggleTiredMode ->
            let
                newState =
                    { model | tiredMode = not model.tiredMode } |> unselectSubject |> Loader.enableLoading
            in
                ( newState, getListRequest model.apiEndpoint newState.tiredMode )


updateSubject : SubjectMsg -> State -> ( State, Cmd Msg )
updateSubject msg model =
    case msg of
        ExpandSubjectClick ( indice, subject ) ->
            let
                detailCmd =
                    Http.send (MySubjectMsg << GetDetail) <| Subject.getDetail model.apiEndpoint subject

                newModel =
                    { model | openedSubjectName = subject.name } |> Loader.enableLoading
            in
                if model.openedSubjectName == subject.name then
                    ( (unselectSubject newModel |> Loader.disableLoading), Cmd.none )
                else
                    ( setOpenedSubject newModel subject.name, detailCmd )

        GetDetail (Ok subject) ->
            ( { model | subjects = Subject.replaceSubjectFromList model.subjects subject } |> Loader.disableLoading, Cmd.none )

        RemoveClick subject ->
            ( model |> Loader.enableLoading, Http.send (MySubjectMsg << Remove) <| Subject.removeRequest model.apiEndpoint subject )

        EditClick subject ->
            let
                newModel =
                    { model
                        | addSubjectModal = True
                        , newSubjectName = subject.name
                        , newWhatToDoNext = subject.whatToDoNext
                        , newPriority = subject.priority
                        , newComplexity = subject.complexity
                    }
            in
                ( newModel, Cmd.none )

        Remove (Ok _) ->
            ( model |> Loader.enableLoading, getListRequest model.apiEndpoint model.tiredMode )

        Remove (Err msg) ->
            errorResult model msg

        GetDetail (Err msg) ->
            errorResult
                model
                msg

        MyDoneMsg a ->
            updateDone a model

        OpenAddSubjectModal ->
            ( { model | addSubjectModal = True } |> unselectSubject, Cmd.none )

        CancelAddSubjectModal ->
            ( { model | addSubjectModal = False }, Cmd.none )

        ChangeNewComplexity complexity ->
            ( { model | newComplexity = complexity }, Cmd.none )

        ChangeNewPriority priority ->
            ( { model | newPriority = priority * 10 }, Cmd.none )

        ChangeNewSubjectName subjectName ->
            ( { model | newSubjectName = subjectName }, Cmd.none )

        ChangeNewWhatToDoNext whatToDoNext ->
            ( { model | newWhatToDoNext = whatToDoNext }, Cmd.none )

        SubmitNewSubject ->
            ( Loader.enableLoading model, Http.send (MySubjectMsg << NewSubjectResult) <| Subject.addSubjectRequest model.apiEndpoint model )

        NewSubjectResult _ ->
            ( { model | addSubjectModal = False, newSubjectName = "" } |> unselectSubject, getListRequest model.apiEndpoint model.tiredMode )


updateDone : DoneMsg -> State -> ( State, Cmd Msg )
updateDone msg model =
    case msg of
        DoneResult (Err msg) ->
            errorResult model msg

        ClickDone subject ->
            ( { model | doneSubjectName = subject.name }, Cmd.none )

        DoneChangeDescription description ->
            ( { model | doneDescription = description }, Cmd.none )

        DoneChangeWhatToDoNext next ->
            ( { model | doneWhatToDoNext = next }, Cmd.none )

        SubmitDone ->
            let
                doneHttp =
                    Http.send (MySubjectMsg << MyDoneMsg << DoneResult) <| Subject.doneRequest model.apiEndpoint model
            in
                ( model |> Loader.enableLoading |> resetCurrentDone, doneHttp )

        CancelDone ->
            ( model |> resetCurrentDone, Cmd.none )

        DoneResult (Ok _) ->
            ( Loader.disableLoading model |> unselectSubject, getListRequest model.apiEndpoint model.tiredMode )


unselectSubject model =
    { model | openedSubjectName = "" }


resetCurrentDone state =
    { state | doneSubjectName = "", doneDescription = "", doneWhatToDoNext = "" }


setOpenedSubject model name =
    { model | openedSubjectName = name }


errorResult : State -> Error -> ( State, Cmd Msg )
errorResult model msg =
    ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )



-- view


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --header container
          div [ css [ displayFlex, justifyContent flexEnd ] ]
            [ div [ css [] ]
                [ a [ css [ padding (px 10) ], href "?page=log" ]
                    [ text "Complete History"
                    ]
                , a [ css [ margin (px 30) ], href "/" ] [ text "Logout" ]
                ]
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ -- conditional loading, modals
              Loader.getLoadingHtml state.loading
            , doneModal state
            , alterSubjectHtml state
            , Toaster.html state.toasterMsg

            -- action menu container
            , div
                [ css [ displayFlex, margin (px 20) ] ]
                [ div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100) ] ]
                    [ div []
                        [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                        , text " Tired mode"
                        ]
                    , button [ buttonCss, onClick (MySubjectMsg OpenAddSubjectModal) ] [ text "Add Subject" ]
                    ]
                ]

            --subject list
            , div []
                [ subjectsToHtml state.openedSubjectName state.subjects
                ]
            ]
        ]


debugBorders css =
    let
        addWidth =
            borderWidth (px 1) :: css

        addStyle =
            borderStyle solid :: addWidth
    in
        addStyle


alterSubjectHtml state =
    case state.addSubjectModal of
        True ->
            div [ modalCss ]
                [ div []
                    [ h1 [] [ text "Subject Settings" ]
                    , div [ css [ marginTop (px 10), marginBottom (px 10) ] ]
                        [ input [ Html.Styled.Attributes.defaultValue state.openedSubjectName, inputCss, type_ "text", placeholder "Subject name", onInput (MySubjectMsg << ChangeNewSubjectName), Html.Styled.Attributes.required True ] []
                        , select [ selectCss, on "change" (Json.Decode.map (MySubjectMsg << ChangeNewPriority) targetValueIntParse) ]
                            (renderPriorityOptions state.newPriority)
                        , select [ selectCss, on "change" (Json.Decode.map (MySubjectMsg << ChangeNewComplexity) targetValueIntParse) ]
                            (renderComplexityOptions <| toString state.newComplexity)
                        , input [ inputCss, type_ "text", placeholder "What to do next", onInput (MySubjectMsg << ChangeNewWhatToDoNext), Html.Styled.Attributes.required True ] []
                        ]
                    , button
                        [ buttonCss, onClick (MySubjectMsg CancelAddSubjectModal) ]
                        [ text "Cancel" ]
                    , button
                        [ buttonCss, onClick (MySubjectMsg SubmitNewSubject) ]
                        [ text "Confirm" ]
                    ]
                ]

        False ->
            emptyNode


renderComplexityOptions defaultValue =
    let
        complexity =
            [ ( "10", "Easy" )
            , ( "50", "Medium" )
            , ( "80", "Hard" )
            , ( "100", "Hardest" )
            ]
    in
        List.map (\option -> optionFromTuple defaultValue option) complexity


renderPriorityOptions defaultValue =
    let
        defaultValueNew =
            defaultValue // 10 |> toString

        priority =
            [ ( "0", "0 - No Priority" )
            , ( "1", "1 - Low Priority" )
            , ( "2", "2" )
            , ( "3", "3" )
            , ( "4", "4" )
            , ( "5", "5 - Medium Priority" )
            , ( "6", "6" )
            , ( "7", "7" )
            , ( "8", "8" )
            , ( "9", "9" )
            , ( "10", "10 - Higest Priority" )
            ]
    in
        List.map (\option -> optionFromTuple defaultValueNew option) priority


optionFromTuple defaultValue ( value, label ) =
    if defaultValue == value then
        option [ Html.Styled.Attributes.value value, Html.Styled.Attributes.selected True ]
            [ text label ]
    else
        option [ Html.Styled.Attributes.value value ]
            [ text label ]


subjectsToHtml : String -> List ( Int, Subject ) -> Html.Styled.Html Msg
subjectsToHtml openedSubjectName list =
    let
        innerList =
            List.map (subjectToHtml openedSubjectName) list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml : String -> ( Int, Subject ) -> Html.Styled.Html Msg
subjectToHtml openedSubjectName ( indice, subject ) =
    li [ onClick ((MySubjectMsg << ExpandSubjectClick) ( indice, subject )), subjectCss openedSubjectName ( indice, subject ), id <| "subject_" ++ toString indice ]
        [ div []
            [ div [ css [ fontSize (Css.em 1.2) ] ]
                [ span [ css [ fontSize (Css.em 0.5), marginRight (px 15) ] ] [ text <| toString (indice + 1) ++ "." ]
                , h1 [ class "noselect", css [ display inline, color defaultColors.textHighlight, marginRight (px 20) ] ] [ text subject.name ]
                , inlineIf (subject.name == openedSubjectName) emptyNode <| inlineInfoOfSubject subject
                , inlineIf (subject.name == openedSubjectName) (doneStart subject) emptyNode
                ]
            , inlineIf (subject.name == openedSubjectName) (hiddenHtml subject) emptyNode
            ]
        ]


inlineIf test ifTrue ifFalse =
    if test then
        ifTrue
    else
        ifFalse


inlineInfoOfSubject subject =
    span [ css [ fontSize (Css.em 0.7) ] ] [ text <| " " ++ toString subject.daysSinceLast ++ " days ago" ]


hiddenHtml subject =
    div [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed NoAction) ]
        [ --properity container
          div [ css [ displayFlex ] ]
            [ div [ css [ displayFlex, justifyContent spaceBetween, width (pct 70), flexWrap wrap ] ]
                [ div []
                    [ subjectProperty "Priority" <| toString subject.priority
                    , subjectProperty "Complexity" <| toString subject.complexity
                    ]
                , div []
                    [ subjectProperty "Days since last session" <| toString subject.daysSinceLast
                    , subjectProperty "Hours already invested" <| subject.timeAlreadyInvested
                    ]
                ]
            ]
        , div
            []
            [ subjectProperty "Next Action" subject.whatToDoNext
            ]
        , div []
            [ h2 [ css [ textAlign center, marginTop (px 50), fontWeight bold ] ] [ text "History" ]
            , div [ css [ margin (px 30) ] ] (List.map studyEntryToHtml subject.history)
            ]
        , button [ buttonCss, onClickStoppingPropagation <| (MySubjectMsg << EditClick) subject ] [ text "Edit" ]
        , button [ buttonCss, onClickStoppingPropagation <| (MySubjectMsg << RemoveClick) subject ] [ text "Remove" ]
        ]


doneStart : Subject -> Html.Styled.Html Msg
doneStart subject =
    div [ css [ Css.float right ] ]
        [ button [ buttonCss, onClickStoppingPropagation <| (MySubjectMsg << MyDoneMsg << ClickDone) subject ] [ text "Done" ]
        ]


onClickStoppingPropagation msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed msg)


subjectProperty name value =
    p [ css [ margin (px 20) ] ]
        [ text <| name ++ ": " ++ value
        ]


doneModal : Subject.DoneData r -> Html Msg
doneModal doneInfo =
    case String.length doneInfo.doneSubjectName of
        0 ->
            emptyNode

        _ ->
            div [ modalCss ]
                [ div []
                    [ h1 [] [ text "Record session" ]
                    , input [ inputCss, type_ "text", placeholder "What was done?", onInput (MySubjectMsg << MyDoneMsg << DoneChangeDescription) ] []
                    , input [ inputCss, type_ "text", placeholder "What is to de done next?", onInput (MySubjectMsg << MyDoneMsg << DoneChangeWhatToDoNext) ] []
                    , div []
                        [ button [ buttonCss, onClick (MySubjectMsg << MyDoneMsg <| CancelDone) ] [ text "Cancel" ]
                        , button [ buttonCss, onClick (MySubjectMsg << MyDoneMsg <| SubmitDone) ] [ text "Confirm" ]
                        ]
                    ]
                ]


inputCss : Attribute Msg
inputCss =
    css [ display block, width (px 300), margin (px 1), marginBottom (px 3), padding (px 10) ]


selectCss =
    css [ display block, width (px 300), marginBottom (px 3), padding (px 4) ]


buttonCss =
    css [ minWidth (px 60), margin (px 3), minHeight (px 30), padding (px 3) ]


subjectCss selectedIndex ( index, subject ) =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), backgroundColor (selectedColor selectedIndex ( index, subject )) ]


selectedColor selectedIndex ( index, subject ) =
    if selectedIndex == subject.name then
        defaultColors.selectedBackground
    else
        defaultColors.normalBackground


studyEntryToHtml : StudyEntry -> Html Msg
studyEntryToHtml studyEntry =
    li [ css [ minHeight (px 30) ] ]
        [ p [ css [ marginTop (px 15), color defaultColors.textHighlight ] ] [ text studyEntry.date ]
        , p [ css [ marginLeft (px 20) ] ] [ text <| studyEntry.description ]
        ]


modalCss =
    css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 1 ]


emptyNode =
    text ""


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none



-- requests


getListRequest endpoint tiredMode =
    let
        url =
            "https://" ++ endpoint ++ "/scheduler" ++ (tiredMode |> Subject.toUrlBool)

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson Subject.decodeSubjectList)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send NewList request



-- decoders
