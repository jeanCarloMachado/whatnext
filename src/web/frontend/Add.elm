module Add exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Css exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Json.Encode
import Http exposing (..)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias PageData =
    { name : String, priority : Int, complexity : Int, apiEndpoint : String }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData "" 50 50 flags.apiEndpoint, Cmd.none )


type Msg
    = None
    | ChangeSubjectName String
    | ChangePriority String
    | ChangeComplexity String
    | Submit PageData
    | SubmitResult (Result Http.Error String)


update msg pageData =
    case msg of
        None ->
            ( pageData, Cmd.none )

        ChangeSubjectName name ->
            ( { pageData | name = name }, Cmd.none )

        ChangeComplexity complexity ->
            let
                value =
                    String.toInt complexity
            in
                case value of
                    Ok int ->
                        ( { pageData | complexity = int }, Cmd.none )

                    Err _ ->
                        ( { pageData | complexity = 50 }, Cmd.none )

        ChangePriority priority ->
            let
                value =
                    String.toInt priority
            in
                case value of
                    Ok int ->
                        ( { pageData | priority = int }, Cmd.none )

                    Err _ ->
                        ( { pageData | priority = 50 }, Cmd.none )

        Submit data ->
            ( data, submitRequest pageData )

        SubmitResult (Ok _) ->
            ( { pageData | name = "", priority = 50, complexity = 50 }, Cmd.none )

        SubmitResult (Err msg) ->
            ( pageData, Cmd.none )


submitRequest : PageData -> Cmd Msg
submitRequest pageData =
    let
        url =
            "https://" ++ pageData.apiEndpoint ++ "/add"

        body =
            Json.Encode.object
                [ ( "name", Json.Encode.string pageData.name )
                , ( "complexity", Json.Encode.int pageData.complexity )
                , ( "priority", Json.Encode.int pageData.priority )
                ]

        request =
            Http.post url (Http.jsonBody body) decodeEmptyResult
    in
        Http.send SubmitResult request


inputCss =
    css [ display block, width (px 300), margin (px 5), padding (px 10) ]


view pageData =
    div []
        [ a [ href "index.html" ]
            [ text "Back"
            ]
        , div []
            [ input [ inputCss, type_ "text", placeholder "Subject name", onInput ChangeSubjectName ] []
            , input [ inputCss, type_ "number", placeholder "Priority", onInput ChangePriority ] []
            , input [ inputCss, type_ "number", placeholder "Complexity", onInput ChangeComplexity ] []
            , button
                [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed (Submit pageData)) ]
                [ text "Confirm" ]
            ]
        ]


subscriptions : PageData -> Sub Msg
subscriptions pageData =
    Sub.none


decodeEmptyResult =
    Json.Decode.succeed ""
