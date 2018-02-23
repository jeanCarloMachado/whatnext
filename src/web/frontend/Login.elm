module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href, type_, css, type_)
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


update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        SubmitForm ->
            case model.pageMode of
                SignupPage ->
                    ( { model | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| request model "signup" )

                LoginPage ->
                    ( { model | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| request model "login" )

        RequestResult (Ok message) ->
            case model.pageMode of
                LoginPage ->
                    ( model |> Loader.disableLoading, Navigation.load "https://app.thewhatnext.net?page=scheduler" )

                SignupPage ->
                    ( { model | pageMode = togglePageMode model.pageMode } |> Loader.disableLoading, Cmd.none )

        RequestResult (Err msg) ->
            let
                newModel =
                    Loader.disableLoading model
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
            ( { model | pageMode = togglePageMode model.pageMode }, Cmd.none )

        None ->
            ( model, Cmd.none )


togglePageMode pageMode =
    case pageMode of
        LoginPage ->
            SignupPage

        SignupPage ->
            LoginPage



-- requests


request model service =
    let
        url =
            "https://" ++ model.apiEndpoint ++ "/" ++ service

        body =
            Json.Encode.object
                [ ( "email", Json.Encode.string model.email )
                , ( "password", Json.Encode.string model.password )
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


view model =
    div [ css [ displayFlex, justifyContent center, alignItems center, height (pct 100), width (pct 100), position fixed ] ]
        [ div [ css [ backgroundColor (Css.hex "ffffff"), padding (px 20), displayFlex, flexDirection column ] ]
            [ h2 [ css [ color defaultColors.textHighlight ] ] [ text <| getPageTitle model.pageMode ]
            , div [ css [ marginTop (px 20), marginBottom (px 20) ] ]
                [ input [ View.inputCss, placeholder "Email", onInput UpdateEmail, type_ "email" ] []
                , input [ View.inputCss, placeholder "Password", type_ "password", onInput UpdatePassword ] []
                ]
            , div []
                [ Toaster.html model.errorMessage
                ]
            , div [ css [ displayFlex, justifyContent flexEnd ] ]
                [ div [ css [] ]
                    [ a [ css [ textDecoration underline, fontSize (Css.em 0.9), padding (px 10), color defaultColors.normalButton ], onClick TogglePageMode ] [ text <| getAccessOtherPageText model.pageMode ]
                    , button [ css View.buttonCss, onClick SubmitForm ] [ text <| getSubmitText model.pageMode ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
