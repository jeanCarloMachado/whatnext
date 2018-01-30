module Log exposing (..)

import Html
import Html.Styled exposing (..)


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


type alias Model =
    { foo : Bool }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model True, Cmd.none )


type Msg
    = None


update msg model =
    ( model, Cmd.none )


view pageData =
    div [] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
