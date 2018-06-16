module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href, type_, css, type_, required)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Css exposing (..)
import Json.Decode
import Json.Encode
import Toaster
import Navigation
import Loader
import View exposing (defaultColors)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type PageMode
    = LoginPage
    | SignupPage


type alias Model =
    { email : String
    , password : String
    , apiEndpoint : String
    , errorMessage : String
    , pageMode : PageMode
    , loading : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" "" flags.apiEndpoint "" LoginPage False, Cmd.none )


type Msg
    = None
    | UpdateEmail String
    | UpdatePassword String
    | SubmitForm
    | RequestResult (Result Http.Error String)
    | TogglePageMode


update msg state =
    case msg of
        UpdateEmail email ->
            ( { state | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { state | password = password }, Cmd.none )

        SubmitForm ->
            case state.pageMode of
                SignupPage ->
                    ( { state | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| request state "signup" )

                LoginPage ->
                    ( { state | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| request state "login" )

        RequestResult (Ok message) ->
            case state.pageMode of
                LoginPage ->
                    ( state, Navigation.load "?page=scheduler" )

                SignupPage ->
                    ( { state | pageMode = togglePageMode state.pageMode } |> Loader.disableLoading, Cmd.none )

        RequestResult (Err msg) ->
            let
                newModel =
                    Loader.disableLoading state
            in
                case msg of
                    Http.BadStatus response ->
                        case (response.body |> Json.Decode.decodeString decodeError) of
                            Ok string ->
                                ( { newModel | errorMessage = string }, Cmd.none )

                            _ ->
                                ( { newModel | errorMessage = "Generic error 2" }, Cmd.none )

                    _ ->
                        ( { newModel | errorMessage = "Generic Error" }, Cmd.none )

        TogglePageMode ->
            ( { state | pageMode = togglePageMode state.pageMode }, Cmd.none )

        None ->
            ( state, Cmd.none )


togglePageMode pageMode =
    case pageMode of
        LoginPage ->
            SignupPage

        SignupPage ->
            LoginPage



-- requests


request state service =
    let
        url =
            state.apiEndpoint ++ "/" ++ service

        body =
            Json.Encode.object
                [ ( "email", Json.Encode.string state.email )
                , ( "password", Json.Encode.string state.password )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = url
            , body = (Http.jsonBody body)
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = True
            }



-- view


decodeError =
    Json.Decode.at [ "message" ] Json.Decode.string


decodeEmptyResult =
    Json.Decode.succeed ""


getPageTitle pageMode =
    case pageMode of
        LoginPage ->
            "Login"

        SignupPage ->
            "SignUp"


getAccessOtherPageText pageMode =
    case pageMode of
        LoginPage ->
            "Not registered?"

        SignupPage ->
            "Already a member?"


getSubmitText pageMode =
    case pageMode of
        LoginPage ->
            "Enter"

        SignupPage ->
            "Join"


view state =
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , height (pct 100)
            , width (pct 100)
            , position fixed
            ]
        ]
        [ Loader.getLoadingHtml state.loading
        , div [ css [ backgroundColor (Css.hex "ffffff"), padding (px 20), displayFlex, flexDirection column ] ]
            [ h2
                [ css
                    [ color defaultColors.textHighlight
                    ]
                ]
                [ text <| getPageTitle state.pageMode ]
            , div
                [ css
                    [ marginTop (px 20)
                    , marginBottom (px 20)
                    ]
                ]
                [ input
                    [ View.inputCss
                    , placeholder "Email"
                    , onInput UpdateEmail
                    , type_ "email"
                    ]
                    []
                , input
                    [ View.inputCss
                    , placeholder "Password"
                    , type_ "password"
                    , onInput UpdatePassword
                    , Html.Styled.Attributes.required True
                    ]
                    []
                ]
            , div [ css [ displayFlex, justifyContent flexEnd ] ]
                [ div [ css [] ]
                    [ a
                        [ css
                            [ textDecoration underline
                            , fontSize (Css.em 0.9)
                            , padding (px 10)
                            , color defaultColors.normalButton
                            ]
                        , onClick TogglePageMode
                        ]
                        [ text <| getAccessOtherPageText state.pageMode ]
                    , button
                        [ css View.buttonCss
                        , onClick SubmitForm
                        ]
                        [ text <| getSubmitText state.pageMode ]
                    ]
                ]

            , getToaster state
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.none


getToaster state =
    let
        message =
            if String.length state.errorMessage > 0 then
                state.errorMessage
            else if String.length state.email < 3 && String.length state.email > 0 then
                "Email is too small"
            else if String.length state.password < 3 && String.length state.password > 0 then
                "The password is too small"
            else
                ""
    in
        div []
            [ Toaster.html message
            ]
