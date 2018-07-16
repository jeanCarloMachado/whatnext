module Done exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue, checked)
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
    , parent : String
    }


init : Flags -> ( State, Cmd Msg )
init flags =
        (initialState flags, Cmd.none )

initialState flags =
    let
        valid = not <| String.isEmpty flags.subjectName
    in
        if String.isEmpty flags.parent then
            State
               ""
               False
               False
               ""
               flags.authToken
               flags.apiEndpoint
               flags.subjectName
               flags.parent
               flags.subjectName
               ""
               valid
               50
               False
         else
            State
               ""
               False
               False
               ""
               flags.authToken
               flags.apiEndpoint
               flags.subjectName
               flags.parent
               flags.parent
               flags.subjectName
               valid
               50
               True

type alias State =
    { errorMessage : String
    , sideMenu : Bool
    , loading : Bool
    , toasterMsg : String
    , authToken : String
    , apiEndpoint : String
    , name : String
    , parent : String
    , title : String
    , description : String
    , formValid : Bool
    , duration : Int
    , archive : Bool
    }

main =
    Html.programWithFlags
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = None
    | ToggleSideMenu
    | ChangeDescription String
    | ChangeSubjectTitle String
    | ChangeDuration String
    | SubmitDone
    | ArchiveToggle
    | DoneResult (Result Http.Error String)
    | ArchiveResult (Result Http.Error String)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        None ->
            ( state, Cmd.none )

        ChangeDescription description ->
            ( { state | description = description }, Cmd.none )

        ChangeSubjectTitle title ->
          let
            newState =  { state | title = title }
          in
            (validateInnerState newState, Cmd.none )

        ChangeDuration duration ->
           let
               durationInt= Result.withDefault 0 (String.toInt duration)
           in
            ({ state | duration = durationInt }, Cmd.none )

        SubmitDone ->
                ( state, Http.send DoneResult <| SDK.doneRequest state state)

        DoneResult _ ->
            case state.archive of
                True -> ( state, Http.send ArchiveResult <| SDK.removeRequest state state )
                False -> ( state, Navigation.back 1 )

        ArchiveResult _ ->
                ( state, Navigation.load "?page=log"  )

        ArchiveToggle ->
            ({state | archive = not state.archive}, Cmd.none)

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
    if (String.length state.name) <= 3 then
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
            label [ css Style.labelCss ] [ text "Title" ]
            , input
                [ Style.inputCss
                , type_ "text"
                , placeholder "Name"
                , Html.Styled.Attributes.required True
                , onInput ChangeSubjectTitle
                , defaultValue state.title
                ]
                []
            , label [] [ text "What was done?" ]
            , textarea
                [ css Style.textAreaCss
                , placeholder "studied x y z"
                , defaultValue state.description
                , onInput ChangeDescription
                ]
                []
            , label [] [ text "Duration" ]
            , input
                [ Style.inputCss
                , type_ "text"
                , placeholder ""
                , Html.Styled.Attributes.required True
                , defaultValue <| toString state.duration
                , onInput ChangeDuration
                ]
                []

            , span [ css [ marginRight (px 10) ] ] [ text "Archive subject" ]
            , label [ class "switch" ]
                [ input [ type_ "checkbox", onClick ArchiveToggle, Html.Styled.Attributes.checked state.archive ] []
                , span [ class "slider" ] []
                ]
            ]

        ]


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none
