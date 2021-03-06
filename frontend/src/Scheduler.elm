module Scheduler exposing (..)

import Json.Decode
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (property, css, href, src, placeholder, type_, id, class, value, required, defaultValue)
import Html.Styled.Events exposing (..)
import Toaster exposing (..)
import Css exposing (..)
import SDK exposing (Subject, PastAction)
import Style exposing (defaultColors)
import Menu
import Navigation


-- json

import Json.Encode


--rest

import Http exposing (..)
import Loader


main =
    Html.programWithFlags
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type alias State =
    { subjects : List Subject
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
    , newObjective : String
    , sideMenu : Bool
    , authToken : String
    }


initialState flags =
    State
        []
        True
        ""
        False
        flags.apiEndpoint
        ""
        ""
        ""
        False
        50
        50
        ""
        ""
        ""
        False
        flags.authToken


init : Flags -> ( State, Cmd Msg )
init flags =
    let
        state =
            initialState flags
    in
        ( state, Http.send NewList <| SDK.getListRequest state state.tiredMode )



-- Model


type Msg
    = NewList (Result Http.Error (List Subject))
    | ToggleTiredMode
    | ToggleSideMenu
    | NoAction


type alias Flags =
    { apiEndpoint : String
    , authToken : String
    }



-- update

update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        NewList (Ok subjects) ->
            ( { model | subjects = subjects } |> Loader.disableLoading, Cmd.none )
        NewList (Err (BadPayload message response)) ->
            case getErrorMessage response of
                Ok messageStr ->
                    if messageStr == "token invalid" 
                    then
                      (model, Navigation.load "?page=login")
                    else
                     SDK.errorResult model message

                _ -> SDK.errorResult model message

        NewList (Err _) ->
            SDK.errorResult model msg

        NoAction ->
            ( model, Cmd.none )

        ToggleSideMenu ->
            ( Menu.toggle model, Cmd.none )

        ToggleTiredMode ->
            let
                newState =
                    { model | tiredMode = not model.tiredMode } |> Loader.enableLoading
            in
                ( newState, Http.send NewList <| SDK.getListRequest newState newState.tiredMode )


getErrorMessage response =
            Json.Decode.decodeString (Json.Decode.field "message" Json.Decode.string) response.body


view : State -> Html.Styled.Html Msg
view state =
    div [ css [ color defaultColors.textNormal ] ]
        [ --- left meu
          Menu.sideBarHtmlOptional state <|
            Menu.sideBarHtml

        --top menu
        , Menu.topBarHtml ToggleSideMenu ""
            [
              tiredButton
              , Style.addButton ""
              , Style.doneButton Nothing
            ]
        , --main content
          div
            [ css [ marginTop (px 50), marginLeft (px 10), marginRight (px 10) ] ]
            [ -- conditional loading, modals
              Loader.getLoadingHtml state.loading
            , Toaster.html state.toasterMsg

            --subject list
            , div []
                [ subjectsToHtml state.subjects
                ]
            ]
        ]


tiredButton = div [
        css [ backgroundColor defaultColors.barColor
            , displayFlex
            , justifyContent spaceBetween
            , flexDirection row
            , minHeight (px 50)
            , alignItems center
            ]

      ] [
            span [ css [ marginRight (px 10), color (Css.hex "ffffff") ] ] [ text "Tired" ]
            ,
            label [ class "switch" ]
                [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                , span [ class "slider" ] []
                ]
              ]


subjectsToHtml : List Subject -> Html.Styled.Html Msg
subjectsToHtml list =
    let
        innerList =
            List.map (subjectToHtml) list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml : Subject -> Html.Styled.Html Msg
subjectToHtml subject =
    li
        [ Style.subjectCss
        ]
        [ a [ href <| "?page=view&subjectName=" ++ subject.name ]
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
                            []
                        , h1
                            [ class "noselect"
                            , css
                                [ display inline
                                , color defaultColors.textHighlight
                                , marginRight (px 20)
                                ]
                            ]
                            [ text subject.name ]
                          , extraInfo subject
                        ]
                    ]
                ]
            ]
        ]


extraInfo subject =
    case subject.daysSinceLast of
      Just days ->
        span
            [ css [ fontSize (Css.em 0.7), color defaultColors.textNormal ]
            ]
            [ text <| " " ++ toString days ++ " days ago" ]
      Nothing ->
        Style.emptyNode


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


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none
