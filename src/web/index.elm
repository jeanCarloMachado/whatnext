module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Platform exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



--Model


type alias Subject =
    { name : String
    , daysSinceLast : Int
    }


subjectList =
    (Subject "initial")



-- Init


init : ( List Subject, Cmd Msg )
init =
    ( [ Subject "now i become death" 666 ], getList )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))


update : Msg -> List Subject -> ( List Subject, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( [], Cmd.none )

        NewList (Ok subjects) ->
            ( subjects, Cmd.none )


getList : Cmd Msg
getList =
    let
        url =
            "http://whatnext:5000"

        request =
            Http.get url decodeAll
    in
        Http.send NewList request


decodeAll : Decoder (List Subject)
decodeAll =
    Json.Decode.list decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)



-- VIEW


subjectToHtml : Subject -> Html Msg
subjectToHtml subject =
    li []
        [ div []
            [ text subject.name
            , text (", " ++ (toString subject.daysSinceLast))
            ]
        ]


subjectsToHtml list =
    let
        innerList =
            List.map subjectToHtml list
    in
        ul [] innerList


view : List Subject -> Html Msg
view list =
    div []
        [ subjectsToHtml list
        ]



-- SUBSCRIPTIONS


subscriptions : List Subject -> Sub Msg
subscriptions model =
    Sub.none
