module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias Model =
    { email : String
    , password : String
    , apiEndpoint : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" "" flags.apiEndpoint, Cmd.none )


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
            ( model, loginRequest model )

        LoginResult (Ok _) ->
            ( model, Cmd.none )

        LoginResult (Err msg) ->
            ( model, Cmd.none )

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
        [ input [ placeholder "Email" ] []
        , input [ placeholder "Password" ] []
        , button [ onClick Login ] [ text "Enter" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
