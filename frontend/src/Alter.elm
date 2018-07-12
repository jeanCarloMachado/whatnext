module Alter exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Css exposing (..)
import Style exposing (defaultColors)
import Html.Styled.Events exposing (..)
import Json.Decode
import Html.Events.Extra exposing (targetValueIntParse)
import Menu
import Loader
import Toaster exposing (..)
import SDK
import Http
import Navigation
import SDK exposing (Subject)
import String


type alias Flags =
    { apiEndpoint : String
    , authToken : String
    , subjectName : String
    }


main =
    Html.programWithFlags
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( State, Cmd Msg )
init flags =
    let
        state =
            State
                ""
                False
                False
                ""
                SDK.emptySubject
                flags.authToken
                flags.apiEndpoint
                False
    in
        if String.isEmpty flags.subjectName then
            ( state, Cmd.none )
        else
            ( state, Http.send GetDetail <| SDK.getDetail state flags.subjectName )


type alias State =
    { errorMessage : String
    , sideMenu : Bool
    , loading : Bool
    , toasterMsg : String
    , subject : Subject
    , authToken : String
    , apiEndpoint : String
    , formValid : Bool
    }


type Msg
    = None
    | ToggleSideMenu
    | ChangeSubjectName String
    | ChangeWhatToDoNext String
    | ChangeObjective String
    | ChangePriority Int
    | ChangeComplexity Int
    | AlterSubjectSubmit
    | NewSubjectResult (Result Http.Error String)
    | GetDetail (Result Http.Error Subject)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )

        ChangeComplexity complexity ->
            ( { state | subject = SDK.setComplexity state.subject complexity }, Cmd.none )

        ChangePriority priority ->
            ( { state | subject = SDK.setPriority state.subject <| priority * 10 }, Cmd.none )

        ChangeSubjectName name ->
            let
                newState =
                    { state | subject = SDK.setName state.subject name }
            in
                ( validateInnerState newState, Cmd.none )

        ChangeWhatToDoNext whatToDoNext ->
            ( { state | subject = SDK.setWhatToDoNext state.subject whatToDoNext }, Cmd.none )

        ChangeObjective objective ->
            ( { state | subject = SDK.setObjective state.subject objective }, Cmd.none )

        AlterSubjectSubmit ->
            ( Loader.enableLoading state
            , Http.send NewSubjectResult <| SDK.addSubjectRequest state state.subject
            )

        NewSubjectResult _ ->
            ( state, Navigation.load "?page=scheduler" )

        GetDetail (Ok subject) ->
            let
                newState =
                    { state | subject = subject } |> Loader.disableLoading
            in
                ( validateInnerState newState, Cmd.none )

        GetDetail (Err msg) ->
            SDK.errorResult
                state
                msg


validateInnerState : State -> State
validateInnerState state =
    { state | formValid = validateState state }


validateState : State -> Bool
validateState state =
    if (String.length state.subject.name) <= 3 then
        False
    else
        True


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml

        --top menu
        , Menu.topBarHtml ToggleSideMenu
            "Alter"
            [ Style.backButton
            , Style.confirmButton (confirmButtonAction state)  state.formValid
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ Loader.getLoadingHtml state.loading
            , Toaster.html state.toasterMsg
            , div []
                [ content state
                ]
            ]
        ]

confirmButtonAction state =
    case state.formValid of
        True ->
            AlterSubjectSubmit
        False ->
            None


content state =
    div []
        [ div
            [ css
                [ displayFlex
                , justifyContent center
                , flexDirection column
                , alignItems center
                ]
            ]
            [ span []
                [ label [ css Style.labelCss ] [ text "Subject name" ]
                , input
                    [ Style.inputCss
                    , type_ "text"
                    , placeholder "Name"
                    , onInput (ChangeSubjectName)
                    , Html.Styled.Attributes.required True
                    , defaultValue state.subject.name
                    ]
                    []
                ]
            , span []
                [ label [ css Style.labelCss ] [ text "Priority" ]
                , select
                    [ css Style.selectCss
                    , on "change" (Json.Decode.map (ChangePriority) targetValueIntParse)
                    ]
                    (renderPriorityOptions state.subject.priority)
                ]
            , span []
                [ label [ css Style.labelCss ] [ text "Complexity" ]
                , select
                    [ css Style.selectCss
                    , on "change" (Json.Decode.map (ChangeComplexity) targetValueIntParse)
                    ]
                    (renderComplexityOptions <| toString state.subject.complexity)
                ]
            , span []
                [ label [ css Style.labelCss ] [ text "Objective" ]
                , textarea
                    [ defaultValue state.subject.objective
                    , css <| List.append Style.textAreaCss [ minHeight (px 35) ]
                    , placeholder "After finishing studying this subject will be able to ..."
                    , onInput (ChangeObjective)
                    , Html.Styled.Attributes.required False
                    ]
                    []
                ]
            , span []
                [ label [] [ text "Next step" ]
                , textarea
                    [ defaultValue state.subject.whatToDoNext
                    , css <| List.append Style.textAreaCss [ minHeight (px 35) ]
                    , placeholder "do x y z"
                    , onInput (ChangeWhatToDoNext)
                    , Html.Styled.Attributes.required False
                    ]
                    []
                ]
            ]
        ]


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none


priority =
    [ ( "0", "No Priority" )
    , ( "2", "Low" )
    , ( "5", "Medium" )
    , ( "7", "High" )
    , ( "10", "Higest" )
    ]


renderPriorityOptions defaultValue =
    let
        defaultValueNew =
            defaultValue // 10 |> toString
    in
        List.map (\option -> Style.optionFromTuple defaultValueNew option) priority


complexity =
    [ ( "10", "Easy" )
    , ( "50", "Medium" )
    , ( "80", "Hard" )
    , ( "100", "Hardest" )
    ]


renderComplexityOptions defaultValue =
    List.map (\option -> Style.optionFromTuple defaultValue option) complexity
