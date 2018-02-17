module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href, type_, css)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Css exposing (..)
import Json.Decode
import Json.Encode
import Toaster
import Navigation


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
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" "" flags.apiEndpoint "" LoginPage, Cmd.none )


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
                    ( { model | errorMessage = "" }, Http.send RequestResult <| request model "signup" )

                LoginPage ->
                    ( { model | errorMessage = "" }, Http.send RequestResult <| request model "login" )

        RequestResult (Ok message) ->
            case model.pageMode of
                LoginPage ->
                    ( model, Navigation.load "https://app.thewhatnext.net?page=scheduler" )

                SignupPage ->
                    ( { model | pageMode = togglePageMode model.pageMode }, Cmd.none )

        RequestResult (Err msg) ->
            case msg of
                Http.BadStatus res ->
                    ( { model | errorMessage = res.status.message }, Cmd.none )

                _ ->
                    ( { model | errorMessage = toString msg }, Cmd.none )

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


decodeEmptyResult =
    Json.Decode.succeed ""


inputCss =
    css [ minWidth (px 300), padding (px 5), margin (px 10) ]


getPageTitle pageMode =
    case pageMode of
        LoginPage ->
            "Login"

        SignupPage ->
            "SignUp"


getAccessOtherPageText pageMode =
    case pageMode of
        LoginPage ->
            "Signup"

        SignupPage ->
            "Login"


getSubmitText pageMode =
    case pageMode of
        LoginPage ->
            "Enter"

        SignupPage ->
            "Join"


view model =
    div [ css [ displayFlex, justifyContent center, alignItems center, height (pct 100), width (pct 100), position fixed ] ]
        [ div [ css [ backgroundColor (Css.hex "ffffff"), padding (px 20), displayFlex, flexDirection column ] ]
            [ h2 [] [ text <| getPageTitle model.pageMode ]
            , input [ inputCss, placeholder "Email", onInput UpdateEmail ] []
            , input [ inputCss, placeholder "Password", type_ "password", onInput UpdatePassword ] []
            , div []
                [ Toaster.html model.errorMessage
                ]
            , div [ css [ displayFlex, justifyContent flexEnd ] ]
                [ div [ css [] ]
                    [ button [ onClick TogglePageMode ] [ text <| getAccessOtherPageText model.pageMode ]
                    , button [ css [ margin (px 10) ], onClick SubmitForm ] [ text <| getSubmitText model.pageMode ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
