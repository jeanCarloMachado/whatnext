module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Http exposing (..)
import Platform exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


main =
    Html.program { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }



--Model


type alias Subject =
    { name : String
    , daysSinceLast : Int
    }


type alias PageData =
    { subjects : List Subject
    , loading : Bool
    }



-- Init


init : ( PageData, Cmd Msg )
init =
    ( PageData [ Subject "now i become death" 666 ] True, getList )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))
    | Done


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( PageData [] False, Cmd.none )

        NewList (Ok subjects) ->
            ( PageData subjects False, Cmd.none )

        Done ->
            ( { model | loading = True }, Cmd.none )


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


subjectToHtml subject =
    li [ css [ borderStyle solid, borderWidth (px 1), marginTop (px 1) ] ]
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
        ul [ css [ listStyle none ] ] innerList


doneMessage : PageData -> String
doneMessage pageData =
    case pageData.loading of
        True ->
            "Loading"

        False ->
            "Done"


view : PageData -> Html.Styled.Html Msg
view pageData =
    div [ css [ margin (pct 3) ] ]
        [ div []
            [ button [ onClick Done ] [ text (doneMessage pageData) ]
            ]
        , div
            []
            [ subjectsToHtml pageData.subjects
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
