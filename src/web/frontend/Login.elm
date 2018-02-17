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
            ( model, Navigation.load "https://app.thewhatnext.net?page=scheduler" )

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
            "https://" ++ model.apiEndpoint ++ "/login"

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
        Http.send LoginResult request


decodeEmptyResult =
    Json.Decode.succeed ""


inputCss =
    css [ minWidth (px 300), padding (px 5), margin (px 10) ]


view model =
    div [ css [ displayFlex, justifyContent center, alignItems center, height (px 300) ] ]
        [ div [ css [ backgroundColor (Css.hex "ffffff"), padding (px 20), displayFlex, flexDirection column ] ]
            [ h2 [] [ text "Login" ]
            , input [ inputCss, placeholder "Email", onInput UpdateEmail ] []
            , input [ inputCss, placeholder "Password", type_ "password", onInput UpdatePassword ] []
            , div [ css [ displayFlex, justifyContent flexEnd ] ]
                [ div [ css [] ]
                    [ a [ href "https://app.thewhatnext.net?page=signup" ] [ text "Sign up" ]
                    , button [ css [ margin (px 10) ], onClick Login ] [ text "Enter" ]
                    ]
                ]
            , div []
                [ Toaster.html model.errorMessage
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
