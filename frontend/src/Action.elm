module Action exposing (..)

import Css exposing (..)
import Navigation
import Html
import Http
import Html.Styled exposing (..)
import Json.Decode.Pipeline
import Json.Decode
import SDK exposing(..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Loader
import Menu
import View exposing (defaultColors)
import Array exposing (Array)

main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }

type alias Flags =
    {
      apiEndpoint : String,
      authToken : String,
      subjectName: String
    }


init : Flags -> ( State, Cmd Msg )
init flags =
    let
      state = State flags.subjectName "" True False flags.apiEndpoint flags.authToken 
    in
      ( state, Cmd.none)



type alias State =
    { subjectName : String
    , toasterMsg : String
    , loading : Bool
    , sideMenu : Bool
    , apiEndpoint : String
    , authToken : String
    }



type Msg
    = None
    | ToggleSideMenu
    | GoToScheduler
    -- | ExpandFutureAction ( Int, FutureAction )
    -- | Remove (Result Http.Error String)
    -- | RemoveClick FutureAction
    -- | GetDetail (Result Http.Error FutureAction)
    -- | MyDoneMsg DoneMsg
    -- | OpenAddModal
    -- | OpedEditModal FutureAction
    -- | CancelAddFutureActionModal
    -- | ChangeFutureActionName String
    -- | ChangeWhatToDoNext String
    -- | ChangeObjective String
    -- | ChangePriority Int
    -- | ChangeComplexity Int
    -- | AlterFutureActionSubmit
    -- | NewFutureActionResult (Result Http.Error String)



-- type DoneMsg
--     = OpenDone FutureAction
--     | DoneResult (Result Http.Error String)
--     | DoneChangeDescription String
--     | DoneChangeWhatToDoNext String
--     | SubmitDone
--     | CancelDone


update : Msg -> State -> (State, Cmd Msg)
update msg state =
    case msg of
        -- HistoryResult (Ok historyList) ->
        --     ( { state | history = historyList, loading = False }, Cmd.none )

        -- HistoryResult (Err msg) ->
        --     ( { state | toasterMsg = toString msg }, Cmd.none )

        GoToScheduler ->
            ( state, Navigation.load "?page=scheduler" )

        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )



-- updateDone : DoneMsg -> State -> ( State, Cmd Msg )
-- updateDone msg model =
--     case msg of
--         OpenDone subject ->
--             ( { model
--                 | doneFutureActionName = subject.name
--                 , doneDescription = subject.whatToDoNext
--               }
--             , Cmd.none
--             )

--         DoneChangeDescription description ->
--             ( { model | doneDescription = description }, Cmd.none )

--         DoneChangeWhatToDoNext next ->
--             ( { model | doneWhatToDoNext = next }, Cmd.none )

--         SubmitDone ->
--             let
--                 doneHttp =
--                     Http.send
--                         (MyFutureActionMsg << MyDoneMsg << DoneResult)
--                     <|
--                         SDK.doneRequest model model
--             in
--                 ( model |> Loader.enableLoading |> resetCurrentDone, doneHttp )

--         CancelDone ->
--             ( model |> resetCurrentDone, Cmd.none )

--         DoneResult (Err msg) ->
--             errorResult model msg

--         DoneResult (Ok _) ->
--             ( Loader.disableLoading model |> unselectFutureAction
--             , Http.send NewList <| SDK.getListRequest model
--             )



-- view

view : State -> Html Msg
view state =
    div [] [ text "gandalf"]




-- viewSubject subject =
--     div [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed NoAction) ]
--         [ --properity container
--           div [ css [ displayFlex ] ]
--             [ div
--                 [ css
--                     [ displayFlex
--                     , justifyContent spaceBetween
--                     , width (pct 70)
--                     , minWidth (px 300)
--                     , flexWrap wrap
--                     ]
--                 ]
--                 [ div []
--                     [ subjectProperty "Priority" <| toString subject.priority
--                     , subjectProperty "Complexity" <| toString subject.complexity
--                     ]
--                 , div []
--                     [ subjectProperty "Last session" <| toString subject.daysSinceLast ++ " days ago"
--                     , subjectProperty "Already invested" <| (toString subject.timeAlreadyInvested) ++ " minutes"
--                     ]
--                 ]
--             ]
--         , div
--             []
--             [ span [ css [ margin (px 20), color defaultColors.textNormal, fontWeight bold ] ] [ text "Objective: " ]
--             , p [ css [ display block, margin (px 30), fontSize (Css.em 0.9) ] ] [ showMultilineText subject.objective ]
--             ]
--         , div
--             []
--             [ span [ css [ margin (px 20), color defaultColors.textNormal, fontWeight bold ] ] [ text "Next Action: " ]
--             , p [ css [ display block, margin (px 30), fontSize (Css.em 0.9) ] ] [ showMultilineText subject.whatToDoNext ]
--             ]
--         , div []
--             [ h2 [ css [ textAlign center, marginTop (px 50), fontWeight bold ] ] [ text "History" ]
--             , div [ css [ margin (px 30) ] ] (List.map pastEntryToHtml subject.history)
--             ]
--         , button
--             [ css View.buttonCss
--             , View.onClickStoppingPropagation <| (MyFutureActionMsg << OpedEditModal) subject
--             ]
--             [ text "Edit" ]
--         , button
--             [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.fail)
--             , View.onClickStoppingPropagation <| (MyFutureActionMsg << RemoveClick) subject
--             ]
--             [ text "Archive" ]
--         ]


-- subscriptions : State -> Sub Msg
-- subscriptions state =
--     Sub.none


-- alterFutureActionHtml : State -> Html.Styled.Html Msg
-- alterFutureActionHtml state =
--     case state.addFutureActionModal of
--         True ->
--             div [ View.modalCss ]
--                 [ div
--                     [ css
--                         [ displayFlex
--                         , justifyContent spaceBetween
--                         , flexDirection column
--                         , alignItems flexStart
--                         , maxHeight (px 650)
--                         , height (pct 100)
--                         ]
--                     ]
--                     [ input
--                         [ defaultValue state.openedFutureActionName
--                         , View.inputCss
--                         , type_ "text"
--                         , placeholder "FutureAction name"
--                         , onInput (MyFutureActionMsg << ChangeFutureActionName)
--                         , Html.Styled.Attributes.required True
--                         ]
--                         []
--                     , select
--                         [ css View.selectCss
--                         , on "change" (Json.Decode.map (MyFutureActionMsg << ChangePriority) targetValueIntParse)
--                         ]
--                         (renderPriorityOptions state.newPriority)
--                     , select
--                         [ css View.selectCss
--                         , on "change" (Json.Decode.map (MyFutureActionMsg << ChangeComplexity) targetValueIntParse)
--                         ]
--                         (renderComplexityOptions <| toString state.newComplexity)
--                     , span []
--                         [ label [ css View.labelCss ] [ text "Objective" ]
--                         , textarea
--                             [ defaultValue state.newObjective
--                             , css <| List.append View.textAreaCss [ minHeight (px 35), height (px 75) ]
--                             , placeholder "After finishing studying this subject will be able to ..."
--                             , onInput (MyFutureActionMsg << ChangeObjective)
--                             , Html.Styled.Attributes.required False
--                             ]
--                             []
--                         ]
--                     , span []
--                         [ label [] [ text "Next step" ]
--                         , textarea
--                             [ defaultValue state.newWhatToDoNext
--                             , css <| List.append View.textAreaCss [ height (px 75), minHeight (px 35) ]
--                             , placeholder "do x y z"
--                             , onInput (MyFutureActionMsg << ChangeWhatToDoNext)
--                             , Html.Styled.Attributes.required False
--                             ]
--                             []
--                         ]
--                     , div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100) ] ]
--                         [ button
--                             [ css View.buttonCss, onClick (MyFutureActionMsg CancelAddFutureActionModal) ]
--                             [ text "Cancel" ]
--                         , button
--                             [ css <| List.append View.buttonCss [ backgroundColor defaultColors.success ]
--                             , onClick (MyFutureActionMsg AlterFutureActionSubmit)
--                             ]
--                             [ text "Confirm" ]
--                         ]
--                     ]
--                 ]

--         False ->
--             View.emptyNode


-- complexity =
--     [ ( "10", "Easy" )
--     , ( "50", "Medium" )
--     , ( "80", "Hard" )
--     , ( "100", "Hardest" )
--     ]


-- renderComplexityOptions defaultValue =
--     List.map (\option -> View.optionFromTuple defaultValue option) complexity


-- priority =
--     [ ( "0", "0 - No Priority" )
--     , ( "1", "1 - Low Priority" )
--     , ( "2", "2" )
--     , ( "3", "3" )
--     , ( "4", "4" )
--     , ( "5", "5 - Medium Priority" )
--     , ( "6", "6" )
--     , ( "7", "7" )
--     , ( "8", "8" )
--     , ( "9", "9" )
--     , ( "10", "10 - Higest Priority" )
--     ]


-- renderPriorityOptions defaultValue =
--     let
--         defaultValueNew =
--             defaultValue // 10 |> toString
--     in
--         List.map (\option -> View.optionFromTuple defaultValueNew option) priority


-- subjectToHtml : String -> ( Int, FutureAction ) -> Html.Styled.Html Msg
-- subjectToHtml openedFutureActionName ( indice, subject ) =
--     li
--         [ onClick ((MyFutureActionMsg << ExpandFutureAction) ( indice, subject ))
--         , subjectCss openedFutureActionName ( indice, subject )
--         , id <| "subject_" ++ subject.name
--         ]
--         [ div []
--             [ div
--                 [ css
--                     [ fontSize (Css.em 1.2)
--                     , displayFlex
--                     , justifyContent spaceBetween
--                     , flexDirection row
--                     , alignItems center
--                     ]
--                 ]
--                 [ div []
--                     [ span
--                         [ css
--                             [ fontSize (Css.em 0.5)
--                             , marginRight (px 15)
--                             ]
--                         ]
--                         [ text <| toString (indice + 1) ++ "." ]
--                     , h1
--                         [ class "noselect"
--                         , css
--                             [ display inline
--                             , color defaultColors.textHighlight
--                             , marginRight (px 20)
--                             ]
--                         ]
--                         [ text subject.name ]
--                     , View.inlineIf (subject.name == openedFutureActionName) View.emptyNode <| inlineInfoOfFutureAction subject
--                     ]
--                 , View.inlineIf (subject.name == openedFutureActionName) (doneStart subject) View.emptyNode
--                 ]
--             , View.inlineIf (subject.name == openedFutureActionName) (hiddenHtml subject) View.emptyNode
--             ]
--         ]

-- doneStart : FutureAction -> Html.Styled.Html Msg
-- doneStart subject =
--     button
--         [ css
--             View.buttonCss
--         , View.onClickStoppingPropagation <|
--             (MyFutureActionMsg
--                 << MyDoneMsg
--                 << OpenDone
--             )
--                 subject
--         ]
--         [ text "Done" ]





-- doneModal : SDK.DoneData r -> Html Msg
-- doneModal doneInfo =
--     case String.length doneInfo.doneFutureActionName of
--         0 ->
--             View.emptyNode

--         _ ->
--             div [ View.modalCss ]
--                 [ div
--                     [ css
--                         [ displayFlex
--                         , justifyContent spaceBetween
--                         , flexDirection column
--                         , alignItems flexStart
--                         , height (pct 100)
--                         , maxHeight (px 500)
--                         ]
--                     ]
--                     [ h1 [ css [ fontSize (Css.em 1.7) ] ] [ text "Record session" ]
--                     , label [] [ text "What was done?" ]
--                     , textarea
--                         [ css View.textAreaCss
--                         , placeholder "studied x y z"
--                         , defaultValue doneInfo.doneDescription
--                         , onInput (MyFutureActionMsg << MyDoneMsg << DoneChangeDescription)
--                         ]
--                         []
--                     , label [] [ text "What to do next" ]
--                     , textarea
--                         [ css View.textAreaCss
--                         , placeholder "study x y z"
--                         , onInput (MyFutureActionMsg << MyDoneMsg << DoneChangeWhatToDoNext)
--                         ]
--                         []
--                     , div [ css [ displayFlex, justifyContent spaceBetween, width (pct 100) ] ]
--                         [ button
--                             [ css View.buttonCss
--                             , onClick (MyFutureActionMsg << MyDoneMsg <| CancelDone)
--                             ]
--                             [ text "Cancel" ]
--                         , button
--                             [ css (View.buttonCss |> View.overrideBackgroundColor defaultColors.success)
--                             , onClick (MyFutureActionMsg << MyDoneMsg <| SubmitDone)
--                             ]
--                             [ text "Confirm" ]
--                         ]
--                     ]
--                 ]

subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none
