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
import Subject exposing (Subject, StudyEntry, DoneData)
import View
import DOM


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
import Navigation
import Loader
import Subject exposing (Subject, StudyEntry, DoneData)


main =
    Html.programWithFlags { init = init, view = view >> Html.Styled.toUnstyled, update = update, subscriptions = subscriptions }


initialState =
    State [] True "" False "" "" "" "" False 50 50 "" "" ""


init : Flags -> ( State, Cmd Msg )
init flags =
    ( initialState |> updateEndpoint flags.apiEndpoint, Http.send NewList <| Subject.getListRequest (initialState |> updateEndpoint flags.apiEndpoint) )


updateEndpoint endpoint state =
    { state | apiEndpoint = endpoint }


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
    | History
    | Logout
    | MySubjectMsg SubjectMsg


type SubjectMsg
    = ExpandSubject ( Int, Subject )
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

        Logout ->
            ( model, Navigation.load "/" )

        History ->
            ( model, Navigation.load "?page=log" )

        ToggleTiredMode ->
            let
                newState =
                    { model | tiredMode = not model.tiredMode } |> unselectSubject |> Loader.enableLoading
            in
                ( newState, Http.send NewList <| Subject.getListRequest newState )


updateSubject : SubjectMsg -> State -> ( State, Cmd Msg )
updateSubject msg model =
    case msg of
        ExpandSubject ( indice, subject ) ->
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
            let
                selector =
                    DOM.idSelector <| "subject_" ++ subject.name

                task =
                    (DOM.scrollIntoView selector)
            in
                ( { model | subjects = Subject.replaceSubjectFromList model.subjects subject } |> Loader.disableLoading, Task.attempt (\a -> (NoAction)) task )

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
            ( model |> Loader.enableLoading, Http.send NewList <| Subject.getListRequest model )

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
            ( { model | addSubjectModal = False, newSubjectName = "" } |> unselectSubject, Http.send NewList <| Subject.getListRequest model )


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
            ( Loader.disableLoading model |> unselectSubject, Http.send NewList <| Subject.getListRequest model )


unselectSubject model =
    { model | openedSubjectName = "" }


resetCurrentDone state =
    { state | doneSubjectName = "", doneDescription = "", doneWhatToDoNext = "" }


setOpenedSubject model name =
    { model | openedSubjectName = name }


errorResult : State -> Error -> ( State, Cmd Msg )
errorResult model msg =
    ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --header container
          div [ css [ displayFlex, justifyContent flexEnd ] ]
            [ div [ css [] ]
                [ button [ css View.buttonCss, onClick History ]
                    [ text "Complete History"
                    ]
                , button [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.fail), onClick Logout ] [ text "Quit" ]
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
                [ div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100), alignItems center ] ]
                    [ div [ css [ displayFlex, alignItems center ] ]
                        [ label [ class "switch" ]
                            [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                            , span [ class "slider" ] []
                            ]
                        , span [ css [ marginLeft (px 10), color defaultColors.textNormal ] ] [ text "Tired mode" ]
                        ]
                    , button [ css View.buttonCss, onClick (MySubjectMsg OpenAddSubjectModal) ] [ text "Add Subject" ]
                    ]
                ]

            --subject list
            , div []
                [ subjectsToHtml state.openedSubjectName state.subjects
                ]
            ]
        ]


alterSubjectHtml state =
    case state.addSubjectModal of
        True ->
            div [ View.modalCss ]
                [ div []
                    [ h1 [] [ text "Subject Settings" ]
                    , div [ css [ marginTop (px 10), marginBottom (px 10) ] ]
                        [ input [ Html.Styled.Attributes.defaultValue state.openedSubjectName, View.inputCss, type_ "text", placeholder "Subject name", onInput (MySubjectMsg << ChangeNewSubjectName), Html.Styled.Attributes.required True ] []
                        , select [ css View.selectCss, on "change" (Json.Decode.map (MySubjectMsg << ChangeNewPriority) targetValueIntParse) ]
                            (renderPriorityOptions state.newPriority)
                        , select [ css View.selectCss, on "change" (Json.Decode.map (MySubjectMsg << ChangeNewComplexity) targetValueIntParse) ]
                            (renderComplexityOptions <| toString state.newComplexity)
                        , input [ View.inputCss, type_ "text", placeholder "What to do next", onInput (MySubjectMsg << ChangeNewWhatToDoNext), Html.Styled.Attributes.required True ] []
                        ]
                    , button
                        [ css View.buttonCss, onClick (MySubjectMsg CancelAddSubjectModal) ]
                        [ text "Cancel" ]
                    , button
                        [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.success), onClick (MySubjectMsg SubmitNewSubject) ]
                        [ text "Confirm" ]
                    ]
                ]

        False ->
            View.emptyNode


renderComplexityOptions defaultValue =
    let
        complexity =
            [ ( "10", "Easy" )
            , ( "50", "Medium" )
            , ( "80", "Hard" )
            , ( "100", "Hardest" )
            ]
    in
        List.map (\option -> View.optionFromTuple defaultValue option) complexity


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
        List.map (\option -> View.optionFromTuple defaultValueNew option) priority


subjectsToHtml : String -> List ( Int, Subject ) -> Html.Styled.Html Msg
subjectsToHtml openedSubjectName list =
    let
        innerList =
            List.map (subjectToHtml openedSubjectName) list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml : String -> ( Int, Subject ) -> Html.Styled.Html Msg
subjectToHtml openedSubjectName ( indice, subject ) =
    li [ onClick ((MySubjectMsg << ExpandSubject) ( indice, subject )), subjectCss openedSubjectName ( indice, subject ), id <| "subject_" ++ subject.name ]
        [ div []
            [ div [ css [ fontSize (Css.em 1.2) ] ]
                [ span [ css [ fontSize (Css.em 0.5), marginRight (px 15) ] ] [ text <| toString (indice + 1) ++ "." ]
                , h1 [ class "noselect", css [ display inline, color defaultColors.textHighlight, marginRight (px 20) ] ] [ text subject.name ]
                , inlineIf (subject.name == openedSubjectName) View.emptyNode <| inlineInfoOfSubject subject
                , inlineIf (subject.name == openedSubjectName) (doneStart subject) View.emptyNode
                ]
            , inlineIf (subject.name == openedSubjectName) (hiddenHtml subject) View.emptyNode
            ]
        ]


inlineIf test ifTrue ifFalse =
    if test then
        ifTrue
    else
        ifFalse


inlineInfoOfSubject subject =
    span [ css [ fontSize (Css.em 0.7), color defaultColors.textNormal ] ] [ text <| " " ++ toString subject.daysSinceLast ++ " days ago" ]


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
            [ span [ css [ margin (px 20), color defaultColors.textNormal ] ] [ text "Next Action: " ]
            , p [ css [ display block, margin (px 30), fontSize (Css.em 0.9) ] ] [ text subject.whatToDoNext ]
            ]
        , div []
            [ h2 [ css [ textAlign center, marginTop (px 50), fontWeight bold ] ] [ text "History" ]
            , div [ css [ margin (px 30) ] ] (List.map studyEntryToHtml subject.history)
            ]
        , button [ css View.buttonCss, View.onClickStoppingPropagation <| (MySubjectMsg << EditClick) subject ] [ text "Edit" ]
        , button [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.fail), View.onClickStoppingPropagation <| (MySubjectMsg << RemoveClick) subject ] [ text "Remove" ]
        ]


doneStart : Subject -> Html.Styled.Html Msg
doneStart subject =
    div [ css [ Css.float right ] ]
        [ button [ css View.buttonCss, View.onClickStoppingPropagation <| (MySubjectMsg << MyDoneMsg << ClickDone) subject ] [ text "Done" ]
        ]


subjectProperty name value =
    div [ css [ margin (px 20) ] ]
        [ span [ css [ color defaultColors.textNormal ] ] [ text <| name ++ ": " ++ value ]
        ]


doneModal : Subject.DoneData r -> Html Msg
doneModal doneInfo =
    case String.length doneInfo.doneSubjectName of
        0 ->
            View.emptyNode

        _ ->
            div [ View.modalCss ]
                [ div []
                    [ h1 [] [ text "Record session" ]
                    , input [ View.inputCss, type_ "text", placeholder "What was done?", onInput (MySubjectMsg << MyDoneMsg << DoneChangeDescription) ] []
                    , input [ View.inputCss, type_ "text", placeholder "What is to de done next?", onInput (MySubjectMsg << MyDoneMsg << DoneChangeWhatToDoNext) ] []
                    , div []
                        [ button [ css View.buttonCss, onClick (MySubjectMsg << MyDoneMsg <| CancelDone) ] [ text "Cancel" ]
                        , button [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.success), onClick (MySubjectMsg << MyDoneMsg <| SubmitDone) ] [ text "Confirm" ]
                        ]
                    ]
                ]


subjectCss selectedIndex ( index, subject ) =
    css
        [ display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), backgroundColor <| Css.rgb 255 255 255, borderStyle none ]


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


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none
