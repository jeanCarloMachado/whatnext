module Signup exposing (..)


import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href, type_, css, type_, required)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Css exposing (..)
import Toaster
import Navigation
import Loader
import Style exposing (defaultColors)
import SDK


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type PageMode
    = LoginPage
    | SignupPage


type alias State =
    { email : String
    , password : String
    , apiEndpoint : String
    , errorMessage : String
    , pageMode : PageMode
    , loading : Bool
    }


init : Flags -> ( State, Cmd Msg )
init flags =
    ( State "" "" flags.apiEndpoint "" LoginPage False, Cmd.none )


type Msg
    = None
    | UpdateEmail String
    | UpdatePassword String
    | SubmitForm
    | RequestResult (Result Http.Error String)


update msg state =
    case msg of
        UpdateEmail email ->
            ( { state | email = email }, Cmd.none )

        UpdatePassword password ->
            ( { state | password = password }, Cmd.none )

        SubmitForm ->
            ( { state | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| SDK.signupRequest state )

        RequestResult (Ok message) ->
            ( state |> Loader.disableLoading, Navigation.load "?page=login" )

        RequestResult (Err msg) ->
            ( { state | errorMessage = "Something went wrong" }, Cmd.none )

        None ->
            ( state, Cmd.none )

-- requests



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
                [ text "Signup" ]
            , div
                [ css
                    [ marginTop (px 20)
                    , marginBottom (px 20)
                    ]
                ]
                [ input
                    [ Style.inputCss
                    , placeholder "Email"
                    , onInput UpdateEmail
                    , type_ "email"
                    ]
                    []
                , input
                    [ Style.inputCss
                    , placeholder "Password"
                    , type_ "password"
                    , onInput UpdatePassword
                    , Html.Styled.Attributes.required True
                    ]
                    []
                , input
                    [ Style.inputCss
                    , placeholder "Repeat password"
                    , type_ "password"
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
                            , href "?page=login"
                        ]
                        [ text "Already a member?" ]
                    , button
                        [ css Style.buttonCss
                        , onClick SubmitForm
                        ]
                        [ text "SignUp" ]
                    ]
                ]

            , getToaster state
            ]
        ]


subscriptions : State -> Sub Msg
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
