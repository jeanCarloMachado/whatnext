module Login exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, href, type_, css, type_, required)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Css exposing (..)
import Style exposing (defaultColors)
import Loader
import SDK
import Navigation
import Storage.Local
import Task

type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias State =
    { email : String
    , password : String
    , apiEndpoint : String
    , errorMessage : String
    , loading : Bool
    , formValid : Bool
    }


init : Flags -> ( State, Cmd Msg )
init flags =
    ( State "" "" flags.apiEndpoint "" False False, Cmd.none )


type Msg
    = None
    | UpdateEmail String
    | UpdatePassword String
    | SubmitForm
    | RequestResult (Result Http.Error SDK.AuthResult)


update msg state =
    case msg of
        UpdateEmail email ->
            ( { state | email = email, formValid = isFormValid state}, Cmd.none )

        UpdatePassword password ->
            ( { state | password = password, formValid = isFormValid state}, Cmd.none )

        SubmitForm ->
            ( { state | errorMessage = "" } |> Loader.enableLoading, Http.send RequestResult <| SDK.loginRequest state )

        RequestResult (Ok authResult) ->
            let
                setStorage = Storage.Local.set "Authorization" authResult.hash
            in
            ( state ,Cmd.batch [Task.attempt (\a -> (None)) setStorage, Navigation.load "?page=scheduler"  ])

        RequestResult (Err msg) ->
              ( { state | errorMessage = "Something went wrong" }, Cmd.none )
        None ->
            ( state, Cmd.none )


isFormValid : State -> Bool
isFormValid state =
    if (String.length state.email > 3 && String.length state.password > 1)
    then
        True
    else
        False

-- view

view : State -> Html Msg
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
                [ text "Login"]
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
                            , href "?page=signup"
                        ]
                        [ text "Not a member?" ]
                    , loginButton state
                    ]
                ]
            ]
        ]


loginButton state =
    case state.formValid of
        True ->
            button
                [ css Style.buttonCss
                , onClick SubmitForm
                ]
                [ text "Login" ]
        False ->
            button
                [ css <| Style.buttonCss ++ [backgroundColor defaultColors.disabledColor ]
                ]
                [ text "Login" ]

subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none


