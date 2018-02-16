module Signup exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode
import Toaster
import Navigation


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias Model =
    { email : String
    , password : String
    , apiEndpoint : String
    , errorMessage : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" "" flags.apiEndpoint "", Cmd.none )


type Msg
    = None
    | UpdateEmail String
    | UpdatePassword String
    | Signup
    | SignupResult (Result Http.Error String)


update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Signup ->
            ( { model | errorMessage = "" }, signupRequest model )

        SignupResult (Ok message) ->
            ( model, Navigation.load "https://app.thewhatnext.net" )

        SignupResult (Err msg) ->
            case msg of
                Http.BadStatus res ->
                    ( { model | errorMessage = res.status.message }, Cmd.none )

                _ ->
                    ( { model | errorMessage = toString msg }, Cmd.none )

        None ->
            ( model, Cmd.none )


signupRequest model =
    let
        url =
            "https://" ++ model.apiEndpoint ++ "/signup"

        body =
            Json.Encode.object
                [ ( "email", Json.Encode.string model.email )
                , ( "password", Json.Encode.string model.password )
                ]

        request =
            Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = (Http.jsonBody body)
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = True
                }
    in
        Http.send SignupResult request


decodeEmptyResult =
    Json.Decode.succeed ""


view model =
    div []
        [ div []
            [ h1 [] [ text "Signup" ]
            , input [ placeholder "Email", onInput UpdateEmail ] []
            , input [ placeholder "Password", onInput UpdatePassword ] []
            , button [ onClick Signup ] [ text "Register" ]
            , a [ href "https://app.thewhatnext.net" ] [ text "Login" ]
            ]
        , div []
            [ Toaster.html model.errorMessage
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
