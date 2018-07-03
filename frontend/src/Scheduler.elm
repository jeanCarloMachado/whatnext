module Scheduler exposing (..)

--view imports

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Html.Styled.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Toaster exposing (..)
import Css exposing (..)
import Subject exposing (Subject, StudyEntry, DoneData)
import View exposing (defaultColors)
import DOM
import Menu
import Keyboard.Combo


-- json

import Json.Decode
import Json.Encode
import Json.Decode exposing (..)


--rest

import Http exposing (..)
import Array exposing (Array)
import Task
import Loader
import Subject exposing (Subject, StudyEntry, DoneData)


main =
    Html.programWithFlags
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


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
    , newObjective : String
    , sideMenu : Bool
    , combos : Keyboard.Combo.Model Msg
    }


initialState =
    State
        []
        True
        ""
        False
        ""
        ""
        ""
        ""
        False
        50
        50
        ""
        ""
        ""
        ""
        False
        (Keyboard.Combo.init keyboardCombos ComboMsg)


init : Flags -> ( State, Cmd Msg )
init flags =
    ( initialState |> updateEndpoint flags.apiEndpoint
    , Http.send NewList <| Subject.getListRequest (initialState |> updateEndpoint flags.apiEndpoint)
    )


updateEndpoint endpoint state =
    { state | apiEndpoint = endpoint }


keyboardCombos : List (Keyboard.Combo.KeyCombo Msg)
keyboardCombos =
    [ Keyboard.Combo.combo2 ( Keyboard.Combo.control, Keyboard.Combo.n ) (MySubjectMsg OpenAddModal)
    ]



-- Model


type Msg
    = NewList (Result Http.Error (Array Subject))
    | ToggleTiredMode
    | NoAction
    | ToggleSideMenu
    | MySubjectMsg SubjectMsg
    | ComboMsg Keyboard.Combo.Msg


type SubjectMsg
    = ExpandSubject ( Int, Subject )
    | Remove (Result Http.Error String)
    | RemoveClick Subject
    | GetDetail (Result Http.Error Subject)
    | MyDoneMsg DoneMsg
    | OpenAddModal
    | OpedEditModal Subject
    | CancelAddSubjectModal
    | ChangeSubjectName String
    | ChangeWhatToDoNext String
    | ChangeObjective String
    | ChangePriority Int
    | ChangeComplexity Int
    | AlterSubjectSubmit
    | NewSubjectResult (Result Http.Error String)


type DoneMsg
    = OpenDone Subject
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

        ToggleSideMenu ->
            ( Menu.toggle model, Cmd.none )

        ToggleTiredMode ->
            let
                newState =
                    { model | tiredMode = not model.tiredMode } |> unselectSubject |> Loader.enableLoading
            in
                ( newState, Http.send NewList <| Subject.getListRequest newState )

        ComboMsg msg ->
            let
                ( updatedKeys, comboCmd ) =
                    Keyboard.Combo.update msg model.combos
            in
                ( { model | combos = updatedKeys }, comboCmd )


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
                ( { model
                    | subjects = Subject.replaceSubjectFromList model.subjects subject
                  }
                    |> Loader.disableLoading
                , Task.attempt (\a -> (NoAction)) task
                )

        RemoveClick subject ->
            ( model |> Loader.enableLoading
            , Http.send (MySubjectMsg << Remove) <| Subject.removeRequest model.apiEndpoint subject
            )

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

        OpenAddModal ->
            let
                newModel =
                    { model
                        | addSubjectModal = True
                        , newObjective = ""
                        , newWhatToDoNext = ""
                    }
            in
                ( newModel |> unselectSubject, Cmd.none )

        OpedEditModal subject ->
            let
                newModel =
                    { model
                        | addSubjectModal = True
                        , newSubjectName = subject.name
                        , newWhatToDoNext = subject.whatToDoNext
                        , newPriority = subject.priority
                        , newComplexity = subject.complexity
                        , newObjective = subject.objective
                    }
            in
                ( newModel, Cmd.none )

        CancelAddSubjectModal ->
            ( { model | addSubjectModal = False }, Cmd.none )

        ChangeComplexity complexity ->
            ( { model | newComplexity = complexity }, Cmd.none )

        ChangePriority priority ->
            ( { model | newPriority = priority * 10 }, Cmd.none )

        ChangeSubjectName subjectName ->
            ( { model | newSubjectName = subjectName }, Cmd.none )

        ChangeWhatToDoNext whatToDoNext ->
            ( { model | newWhatToDoNext = whatToDoNext }, Cmd.none )

        ChangeObjective objective ->
            ( { model | newObjective = objective }, Cmd.none )

        AlterSubjectSubmit ->
            ( Loader.enableLoading model
            , Http.send (MySubjectMsg << NewSubjectResult) <| Subject.addSubjectRequest model.apiEndpoint model
            )

        NewSubjectResult _ ->
            ( { model | addSubjectModal = False, newSubjectName = "" } |> unselectSubject
            , Http.send NewList <| Subject.getListRequest model
            )


updateDone : DoneMsg -> State -> ( State, Cmd Msg )
updateDone msg model =
    case msg of
        OpenDone subject ->
            ( { model
                | doneSubjectName = subject.name
                , doneDescription = subject.whatToDoNext
              }
            , Cmd.none
            )

        DoneChangeDescription description ->
            ( { model | doneDescription = description }, Cmd.none )

        DoneChangeWhatToDoNext next ->
            ( { model | doneWhatToDoNext = next }, Cmd.none )

        SubmitDone ->
            let
                doneHttp =
                    Http.send
                        (MySubjectMsg << MyDoneMsg << DoneResult)
                    <|
                        Subject.doneRequest model.apiEndpoint model
            in
                ( model |> Loader.enableLoading |> resetCurrentDone, doneHttp )

        CancelDone ->
            ( model |> resetCurrentDone, Cmd.none )

        DoneResult (Err msg) ->
            errorResult model msg

        DoneResult (Ok _) ->
            ( Loader.disableLoading model |> unselectSubject
            , Http.send NewList <| Subject.getListRequest model
            )


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
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml ToggleSideMenu

        --top menu
        , Menu.topBarHtml ToggleSideMenu
            [ span [ css [ marginRight (px 10), color (Css.hex "ffffff") ] ] [ text "Tired" ]
            , label [ class "switch" ]
                [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                , span [ class "slider" ] []
                ]
            , img
                [ css
                    [ marginLeft (px 30)
                    , marginRight (px 10)
                    , maxHeight (px 55)
                    ]
                , src "images/add.png"
                , onClick (MySubjectMsg OpenAddModal)
                ]
                []
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ -- conditional loading, modals
              Loader.getLoadingHtml state.loading
            , doneModal state
            , alterSubjectHtml state
            , Toaster.html state.toasterMsg

            --subject list
            , div []
                [ subjectsToHtml state.openedSubjectName state.subjects
                ]
            ]
        ]


alterSubjectHtml : State -> Html.Styled.Html Msg
alterSubjectHtml state =
    case state.addSubjectModal of
        True ->
            div [ View.modalCss ]
                [ div
                    [ css
                        [ displayFlex
                        , justifyContent spaceBetween
                        , flexDirection column
                        , alignItems flexStart
                        , maxHeight (px 650)
                        , height (pct 100)
                        ]
                    ]
                    [ input
                        [ defaultValue state.openedSubjectName
                        , View.inputCss
                        , type_ "text"
                        , placeholder "Subject name"
                        , onInput (MySubjectMsg << ChangeSubjectName)
                        , Html.Styled.Attributes.required True
                        ]
                        []
                    , select
                        [ css View.selectCss
                        , on "change" (Json.Decode.map (MySubjectMsg << ChangePriority) targetValueIntParse)
                        ]
                        (renderPriorityOptions state.newPriority)
                    , select
                        [ css View.selectCss
                        , on "change" (Json.Decode.map (MySubjectMsg << ChangeComplexity) targetValueIntParse)
                        ]
                        (renderComplexityOptions <| toString state.newComplexity)
                    , span []
                        [ label [ css View.labelCss ] [ text "Objective" ]
                        , textarea
                            [ defaultValue state.newObjective
                            , css <| List.append View.textAreaCss [ minHeight (px 35), height (px 75) ]
                            , placeholder "After finishing studying this subject will be able to ..."
                            , onInput (MySubjectMsg << ChangeObjective)
                            , Html.Styled.Attributes.required False
                            ]
                            []
                        ]
                    , span []
                        [ label [] [ text "Next step" ]
                        , textarea
                            [ defaultValue state.newWhatToDoNext
                            , css <| List.append View.textAreaCss [ height (px 75), minHeight (px 35) ]
                            , placeholder "do x y z"
                            , onInput (MySubjectMsg << ChangeWhatToDoNext)
                            , Html.Styled.Attributes.required False
                            ]
                            []
                        ]
                    , div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100) ] ]
                        [ button
                            [ css View.buttonCss, onClick (MySubjectMsg CancelAddSubjectModal) ]
                            [ text "Cancel" ]
                        , button
                            [ css <| List.append View.buttonCss [ backgroundColor defaultColors.success ]
                            , onClick (MySubjectMsg AlterSubjectSubmit)
                            ]
                            [ text "Confirm" ]
                        ]
                    ]
                ]

        False ->
            View.emptyNode


complexity =
    [ ( "10", "Easy" )
    , ( "50", "Medium" )
    , ( "80", "Hard" )
    , ( "100", "Hardest" )
    ]

renderComplexityOptions defaultValue =
    List.map (\option -> View.optionFromTuple defaultValue option) complexity

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


renderPriorityOptions defaultValue =
    let
        defaultValueNew =
            defaultValue // 10 |> toString
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
    li
        [ onClick ((MySubjectMsg << ExpandSubject) ( indice, subject ))
        , subjectCss openedSubjectName ( indice, subject )
        , id <| "subject_" ++ subject.name
        ]
        [ div []
            [ div
                [ css
                    [ fontSize (Css.em 1.2)
                    , displayFlex
                    , justifyContent spaceBetween
                    , flexDirection row
                    , alignItems center
                    ]
                ]
                [ div []
                    [ span
                        [ css
                            [ fontSize (Css.em 0.5)
                            , marginRight (px 15)
                            ]
                        ]
                        [ text <| toString (indice + 1) ++ "." ]
                    , h1
                        [ class "noselect"
                        , css
                            [ display inline
                            , color defaultColors.textHighlight
                            , marginRight (px 20)
                            ]
                        ]
                        [ text subject.name ]
                    , View.inlineIf (subject.name == openedSubjectName) View.emptyNode <| inlineInfoOfSubject subject
                    ]
                , View.inlineIf (subject.name == openedSubjectName) (doneStart subject) View.emptyNode
                ]
            , View.inlineIf (subject.name == openedSubjectName) (hiddenHtml subject) View.emptyNode
            ]
        ]


doneStart : Subject -> Html.Styled.Html Msg
doneStart subject =
    button
        [ css
            View.buttonCss
        , View.onClickStoppingPropagation <|
            (MySubjectMsg
                << MyDoneMsg
                << OpenDone
            )
                subject
        ]
        [ text "Done" ]


inlineInfoOfSubject subject =
    span
        [ css [ fontSize (Css.em 0.7), color defaultColors.textNormal ]
        ]
        [ text <| " " ++ toString subject.daysSinceLast ++ " days ago" ]


hiddenHtml subject =
    div [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed NoAction) ]
        [ --properity container
          div [ css [ displayFlex ] ]
            [ div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , width (pct 70)
                    , minWidth (px 300)
                    , flexWrap wrap
                    ]
                ]
                [ div []
                    [ subjectProperty "Priority" <| toString subject.priority
                    , subjectProperty "Complexity" <| toString subject.complexity
                    ]
                , div []
                    [ subjectProperty "Last session" <| toString subject.daysSinceLast ++ " days ago"
                    , subjectProperty "Already invested" <| (toString subject.timeAlreadyInvested) ++ " minutes"
                    ]
                ]
            ]
        , div
            []
            [ span [ css [ margin (px 20), color defaultColors.textNormal, fontWeight bold ] ] [ text "Objective: " ]
            , p [ css [ display block, margin (px 30), fontSize (Css.em 0.9) ] ] [ showMultilineText subject.objective ]
            ]
        , div
            []
            [ span [ css [ margin (px 20), color defaultColors.textNormal, fontWeight bold ] ] [ text "Next Action: " ]
            , p [ css [ display block, margin (px 30), fontSize (Css.em 0.9) ] ] [ showMultilineText subject.whatToDoNext ]
            ]
        , div []
            [ h2 [ css [ textAlign center, marginTop (px 50), fontWeight bold ] ] [ text "History" ]
            , div [ css [ margin (px 30) ] ] (List.map studyEntryToHtml subject.history)
            ]
        , button
            [ css View.buttonCss
            , View.onClickStoppingPropagation <| (MySubjectMsg << OpedEditModal) subject
            ]
            [ text "Edit" ]
        , button
            [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.fail)
            , View.onClickStoppingPropagation <| (MySubjectMsg << RemoveClick) subject
            ]
            [ text "Archive" ]
        ]


showMultilineText str =
    let
        newStr =
            String.split "\n" str
                |> String.join "</br>"
    in
        span [ Html.Styled.Attributes.property "innerHTML" (Json.Encode.string newStr) ] []


subjectProperty name value =
    div [ css [ margin (px 20) ] ]
        [ span
            [ css [ color defaultColors.textNormal, fontWeight bold ]
            ]
            [ text <| name ++ ": " ]
        , span [] [ text value ]
        ]


doneModal : Subject.DoneData r -> Html Msg
doneModal doneInfo =
    case String.length doneInfo.doneSubjectName of
        0 ->
            View.emptyNode

        _ ->
            div [ View.modalCss ]
                [ div
                    [ css
                        [ displayFlex
                        , justifyContent spaceBetween
                        , flexDirection column
                        , alignItems flexStart
                        , height (pct 100)
                        , maxHeight (px 500)
                        ]
                    ]
                    [ h1 [ css [ fontSize (Css.em 1.7) ] ] [ text "Record session" ]
                    , label [] [ text "What was done?" ]
                    , textarea
                        [ css View.textAreaCss
                        , placeholder "studied x y z"
                        , defaultValue doneInfo.doneDescription
                        , onInput (MySubjectMsg << MyDoneMsg << DoneChangeDescription)
                        ]
                        []
                    , label [] [ text "What to do next" ]
                    , textarea
                        [ css View.textAreaCss
                        , placeholder "study x y z"
                        , onInput (MySubjectMsg << MyDoneMsg << DoneChangeWhatToDoNext)
                        ]
                        []
                    , div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100) ] ]
                        [ button
                            [ css View.buttonCss
                            , onClick (MySubjectMsg << MyDoneMsg <| CancelDone)
                            ]
                            [ text "Cancel" ]
                        , button
                            [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.success)
                            , onClick (MySubjectMsg << MyDoneMsg <| SubmitDone)
                            ]
                            [ text "Confirm" ]
                        ]
                    ]
                ]


subjectCss selectedIndex ( index, subject ) =
    css
        [ display block
        , borderWidth (px 1)
        , padding (px 20)
        , marginBottom (px 1)
        , backgroundColor <| Css.rgb 255 255 255
        , borderStyle none
        ]


selectedColor selectedIndex ( index, subject ) =
    if selectedIndex == subject.name then
        defaultColors.selectedBackground
    else
        defaultColors.normalBackground


studyEntryToHtml : StudyEntry -> Html Msg
studyEntryToHtml studyEntry =
    li [ css [ minHeight (px 30) ] ]
        [ p
            [ css
                [ marginTop (px 15)
                , color defaultColors.textHighlight
                ]
            ]
            [ text studyEntry.date ]
        , p [ css [ marginLeft (px 20) ] ] [ showMultilineText <| studyEntry.description ]
        ]


subscriptions : State -> Sub Msg
subscriptions model =
    Keyboard.Combo.subscriptions model.combos
