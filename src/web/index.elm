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
type alias Model =
    {
        name: String
    }

-- Init
init : (Model, Cmd Msg)
init =
    (Model "now i become death",  getList)


-- UPDATE

type Msg
 = Empty | NewList (Result Http.Error Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewList (Err msg) ->
            ( {  model | name = toString msg}, Cmd.none)
        NewList (Ok newModel) ->
            (newModel, Cmd.none)
        _ ->
            (model, Cmd.none)

getList :  Cmd Msg
getList =
    let
        url =
          "http://whatnext:5000"
        request =
            Http.get url decodeSubjects
    in
        Http.send NewList request

decodeSubjects : Decoder Model
decodeSubjects =
    at [ "0" ] decodeSubject


decodeSubject : Decoder Model
decodeSubject =
        Json.Decode.Pipeline.decode Model
        |>  Json.Decode.Pipeline.required "name" (Json.Decode.string)

-- VIEW
view : Model -> Html Msg
view model =
    div []
    [
        ul []
            [
                li [] [text model.name]
            ]
    ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
