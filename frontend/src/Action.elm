module Action exposing (..)

import Navigation
import Html
import Http
import Html.Styled exposing (..)
import SDK exposing (FutureAction, PastAction)
import Loader
import Menu
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Css exposing (..)
import View exposing (defaultColors)
import Toaster exposing (..)
import Json.Encode


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias Flags =
    { apiEndpoint : String
    , authToken : String
    , subjectName : String
    }


init : Flags -> ( State, Cmd Msg )
init flags =
    let
        state =
            State
                flags.subjectName
                ""
                True
                False
                flags.apiEndpoint
                flags.authToken
                Nothing
    in
        ( state, Http.send GetDetail <| SDK.getDetail state flags.subjectName )


type alias State =
    { subjectName : String
    , toasterMsg : String
    , loading : Bool
    , sideMenu : Bool
    , apiEndpoint : String
    , authToken : String
    , subject : Maybe FutureAction
    }


type Msg
    = None
    | ToggleSideMenu
    | GoToScheduler
    | GetDetail (Result Http.Error FutureAction)



-- | Remove (Result Http.Error String)
-- | RemoveClick FutureAction
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


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        GoToScheduler ->
            ( state, Navigation.load "?page=scheduler" )

        ToggleSideMenu ->
            ( Menu.toggle state, Cmd.none )

        GetDetail (Ok subject) ->
            ( { state | subject = Just subject } |> Loader.disableLoading, Cmd.none )

        GetDetail (Err msg) ->
            errorResult
                state
                msg

        None ->
            ( state, Cmd.none )


errorResult model msg =
    ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )



-- view


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml ToggleSideMenu

        --top menu
        , Menu.topBarHtml ToggleSideMenu
            []
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ -- conditional loading, modals
              Loader.getLoadingHtml state.loading
            , Toaster.html state.toasterMsg
            , div []
                [ viewSubject state.subject
                ]
            ]
        ]


viewSubject : Maybe FutureAction -> Html.Styled.Html Msg
viewSubject msubject =
    case msubject of
        Nothing ->
            View.emptyNode

        Just subject ->
            div
                [ subjectCss ]
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
                            [ h1
                                [ class "noselect"
                                , css
                                    [ display inline
                                    , color defaultColors.textHighlight
                                    , marginRight (px 20)
                                    ]
                                ]
                                [ text subject.name ]
                            ]
                        ]
                    , div []
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
                            , div [ css [ margin (px 30) ] ] (List.map pastEntryToHtml subject.history)
                            ]
                        ]
                    ]
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


pastEntryToHtml : PastAction -> Html Msg
pastEntryToHtml pastEntry =
    li [ css [ minHeight (px 30) ] ]
        [ p
            [ css
                [ marginTop (px 15)
                , color defaultColors.textHighlight
                ]
            ]
            [ text pastEntry.date ]
        , p [ css [ marginLeft (px 20) ] ] [ showMultilineText <| pastEntry.description ]
        ]


subjectCss =
    css
        [ display block
        , borderWidth (px 1)
        , padding (px 20)
        , marginBottom (px 1)
        , backgroundColor <| Css.rgb 255 255 255
        , borderStyle none
        ]


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none
