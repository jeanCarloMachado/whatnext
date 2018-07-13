module Done exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Css exposing (..)
import Style exposing (defaultColors)
import Html.Styled.Events exposing (..)
import Menu
import Loader
import Toaster exposing (..)
import SDK
import Http
import Navigation
import SDK exposing (Subject)

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
        valid = not <| String.isEmpty flags.subjectName
        state =
            State
               ""
               False
               False
               ""
               flags.authToken
               flags.apiEndpoint
               flags.subjectName
               ""
               ""
               valid
               50
    in
        ( state, Cmd.none )


type alias State =
    { errorMessage : String
    , sideMenu : Bool
    , loading : Bool
    , toasterMsg : String
    , authToken : String
    , apiEndpoint : String
    , subjectName : String
    , description : String
    , whatToDoNext : String
    , formValid : Bool
    , duration : Int
    }


type Msg
    = None
    | ToggleSideMenu
    | ChangeDescription String
    | ChangeWhatToDoNext String
    | ChangeSubjectName String
    | ChangeDuration String
    | SubmitDone
    | DoneResult (Result Http.Error String)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )

        ChangeDescription description ->
            ( { state | description = description }, Cmd.none )

        ChangeWhatToDoNext description ->
            ( { state | whatToDoNext = description }, Cmd.none )

        ChangeSubjectName name ->
          let
            newState =  { state | subjectName = name }
          in
            (validateInnerState newState, Cmd.none )

        ChangeDuration duration ->
           let
               durationInt= Result.withDefault 0 (String.toInt duration) 
           in
            ({ state | duration = durationInt }, Cmd.none )

        SubmitDone ->
            let
                request =
                    SDK.doneRequest state state
            in
                ( state, Http.send DoneResult request )

        DoneResult _ ->
            ( state, Navigation.load "?page=log" )


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml

        --top menu
        , Menu.topBarHtml ToggleSideMenu "Done"
            [
                Style.backButton
                , Style.confirmButton (confirmButtonAction state)  state.formValid
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ Loader.getLoadingHtml state.loading
            , Toaster.html state.toasterMsg
            , div []
                [ content state ]
            ]
        ]

confirmButtonAction state =
    case state.formValid of
        True ->
            SubmitDone
        False ->
            None

validateInnerState : State -> State
validateInnerState state =
    { state | formValid = validateState state }


validateState : State -> Bool
validateState state =
    if (String.length state.subjectName) <= 3 then
        False
    else
        True


content : State -> Html.Styled.Html Msg
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
            [
            label [ css Style.labelCss ] [ text "Subject name" ]
            , input
                [ Style.inputCss
                , type_ "text"
                , placeholder "Name"
                , Html.Styled.Attributes.required True
                , onInput ChangeSubjectName
                , defaultValue state.subjectName
                ]
                []
            , label [] [ text "What was done?" ]
            , textarea
                [ css Style.textAreaCss
                , placeholder "studied x y z"
                , onInput ChangeDescription
                ]
                []
            , label [] [ text "What to do next" ]
            , textarea
                [ css Style.textAreaCss
                , placeholder "study x y z"
                , onInput ChangeWhatToDoNext
                ]
                []

            , label [] [ text "Duration in minutes" ]
            , input
                [ Style.inputCss
                , type_ "text"
                , placeholder ""
                , Html.Styled.Attributes.required True
                , defaultValue <| toString state.duration
                , onInput ChangeDuration
                ]
                []

            ]

        ]


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none
