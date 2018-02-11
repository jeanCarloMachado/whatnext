module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder)
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
    | Login
    | LoginResult (Result Http.Error String)


update msg model =
    case msg of
        UpdateEmail email ->
            ( { model | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( { model | errorMessage = "" }, loginRequest model )

        LoginResult (Ok message) ->
            ( model, Navigation.newUrl "http://google.com" )

        LoginResult (Err msg) ->
            case msg of
                Http.BadStatus res ->
                    ( { model | errorMessage = res.status.message }, Cmd.none )

                _ ->
                    ( { model | errorMessage = toString msg }, Cmd.none )

        None ->
            ( model, Cmd.none )


loginRequest model =
    let
        url =
            "http://" ++ model.apiEndpoint ++ "/login"

        body =
            Json.Encode.object
                [ ( "email", Json.Encode.string model.email )
                , ( "password", Json.Encode.string model.password )
                ]

        request =
            Http.post url (Http.jsonBody body) decodeEmptyResult
    in
        Http.send LoginResult request


decodeEmptyResult =
    Json.Decode.succeed ""


view model =
    div []
        [ div []
            [ input [ placeholder "Email", onInput UpdateEmail ] []
            , input [ placeholder "Password", onInput UpdatePassword ] []
            , button [ onClick Login ] [ text "Enter" ]
            ]
        , div []
            [ Toaster.html model.errorMessage
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
