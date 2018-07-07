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


type alias Flags =
    { apiEndpoint : String
    , authToken : String
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
    ( State "" False False 50 "" 50 "" "" "" "" flags.authToken flags.apiEndpoint, Cmd.none )

type alias State =
    {
    errorMessage : String
    , sideMenu : Bool
    , loading : Bool
    , newComplexity : Int
    , newFutureActionName : String
    , newPriority : Int
    , newSubjectName : String
    , newWhatToDoNext : String
    , newObjective: String
    , toasterMsg : String
    , authToken : String
    , apiEndpoint : String
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

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )
        None ->
            ( state, Cmd.none )
        ChangeComplexity complexity ->
            ( { state | newComplexity = complexity }, Cmd.none )

        ChangePriority priority ->
            ( { state | newPriority = priority * 10 }, Cmd.none )

        ChangeSubjectName subjectName ->
            ( { state | newSubjectName = subjectName }, Cmd.none )

        ChangeWhatToDoNext whatToDoNext ->
            ( { state | newWhatToDoNext = whatToDoNext }, Cmd.none )

        ChangeObjective objective ->
            ( { state | newObjective = objective }, Cmd.none )

        AlterSubjectSubmit ->
            ( Loader.enableLoading state
            , Http.send NewSubjectResult <| SDK.addFutureActionRequest state state
            )
        NewSubjectResult _ ->
            ( state, Navigation.load "?page=scheduler" )

view : State -> Html.Styled.Html Msg
view state =
      div [ css [ color defaultColors.textNormal ] ]
          [ --- left meu
            Menu.sideBarHtmlOptional state <|
              Menu.sideBarHtml ToggleSideMenu
          --top menu
          , Menu.topBarHtml ToggleSideMenu
              [
                button
                [ css <| List.append Style.buttonCss [ backgroundColor defaultColors.success ]
                , onClick (AlterSubjectSubmit)
                ]
                [ text "Confirm" ]
              ]
          , --main content
            div
              [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
              [
                Loader.getLoadingHtml state.loading
              , Toaster.html state.toasterMsg
              , div []
                  [ content state
                  ]
              ]
          ]


content state = div []
                [ div
                    [ css
                        [ displayFlex
                        , justifyContent center
                        , flexDirection column
                        , alignItems center
                        ]
                    ]
                    [ input
                        [
                        Style.inputCss
                        , type_ "text"
                        , placeholder "Subject name"
                        , onInput (ChangeSubjectName)
                        , Html.Styled.Attributes.required True
                        ]
                        []
                    , select
                        [ css Style.selectCss
                        , on "change" (Json.Decode.map (ChangePriority) targetValueIntParse)
                        ]
                        (renderPriorityOptions state.newPriority)
                    , select
                        [ css Style.selectCss
                        , on "change" (Json.Decode.map (ChangeComplexity) targetValueIntParse)
                        ]
                        (renderComplexityOptions <| toString state.newComplexity)
                    , span []
                        [ label [ css Style.labelCss ] [ text "Objective" ]
                        , textarea
                            [ defaultValue state.newObjective
                            , css <| List.append Style.textAreaCss [ minHeight (px 35)]
                            , placeholder "After finishing studying this subject will be able to ..."
                            , onInput (ChangeObjective)
                            , Html.Styled.Attributes.required False
                            ]
                            []
                        ]
                    , span []
                        [ label [] [ text "Next step" ]
                        , textarea
                            [ defaultValue state.newWhatToDoNext
                            , css <| List.append Style.textAreaCss [  minHeight (px 35) ]
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
        List.map (\option -> Style.optionFromTuple defaultValueNew option) priority



complexity =
    [ ( "10", "Easy" )
    , ( "50", "Medium" )
    , ( "80", "Hard" )
    , ( "100", "Hardest" )
    ]

renderComplexityOptions defaultValue =
    List.map (\option -> Style.optionFromTuple defaultValue option) complexity
