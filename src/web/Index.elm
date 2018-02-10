module Index exposing (..)

import Html
import Html.Styled
import Http exposing (..)
import Platform exposing (..)
import Json.Encode
import View exposing (..)
import Models exposing (..)
import Array exposing (Array)
import Dom.Scroll
import Task


type alias Flags =
    { apiEndpoint : String }


main =
    Html.programWithFlags { init = init, view = view >> Html.Styled.toUnstyled, update = update, subscriptions = subscriptions }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] Nothing True "" False flags.apiEndpoint, Models.getListRequest flags.apiEndpoint False )


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Ok subjects) ->
            ( { model | subjects = Array.toIndexedList subjects, loading = False }, Cmd.none )

        ExpandSubjectClick ( indice, subject ) ->
            case model.openedIndex of
                Just indexVal ->
                    if indexVal == indice then
                        ( { model | loading = False, openedIndex = Nothing }, Cmd.none )
                    else
                        getDetailUpdateResult model indice subject

                Nothing ->
                    getDetailUpdateResult model indice subject

        GetDetail (Ok subject) ->
            let
                newModel =
                    (replaceSubjectFromList model subject)
            in
                ( { newModel | loading = False }, Task.attempt (always None) <| Dom.Scroll.toTop ("subject_" ++ toString (getOffsetOfSubject model.subjects subject)) )

        ClickDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = True }), Cmd.none )

        CancelDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = False }), Cmd.none )

        DoneChangeDescription subject description ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | description = description }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( (replaceSubjectFromList model newSubject), Cmd.none )

        DoneChangeWhatToDoNext subject next ->
            let
                doneData =
                    subject.doneData

                newDoneData =
                    { doneData | whatToDoNext = next }

                newSubject =
                    { subject | doneData = newDoneData }
            in
                ( replaceSubjectFromList model newSubject, Cmd.none )

        SubmitDone subject ->
            ( { model | loading = True }, Models.doneRequest model.apiEndpoint subject )

        DoneResult (Ok _) ->
            ( { model | loading = False }, Models.getListRequest model.apiEndpoint model.tiredMode )

        ToggleTiredMode ->
            ( { model | tiredMode = not model.tiredMode }, Models.getListRequest model.apiEndpoint <| not model.tiredMode )

        RemoveClick subject ->
            ( { model | loading = True }, Models.removeRequest model.apiEndpoint subject )

        Remove (Ok _) ->
            ( { model | loading = True }, Models.getListRequest model.apiEndpoint model.tiredMode )

        Remove (Err msg) ->
            errorResult model msg

        DoneResult (Err msg) ->
            errorResult model msg

        GetDetail (Err msg) ->
            errorResult model msg

        NewList (Err msg) ->
            errorResult model msg

        None ->
            ( model, Cmd.none )


getOffsetOfSubject : List ( Int, Subject ) -> Subject -> Int
getOffsetOfSubject subjects subject =
    let
        filtered =
            List.filter (\x -> subject.name == (Tuple.second x).name) subjects
    in
        case filtered of
            [ a ] ->
                Tuple.first a

            _ ->
                0


getDetailUpdateResult model indice subject =
    ( { model | loading = True, openedIndex = Just indice }, Models.getDetail model.apiEndpoint subject )


errorResult : PageData -> Error -> ( PageData, Cmd Msg )
errorResult model msg =
    ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )


replaceSubjectFromList : PageData -> Subject -> PageData
replaceSubjectFromList pageData subject =
    let
        newList =
            (List.map (\x -> replaceSame subject x) pageData.subjects)
    in
        { pageData | subjects = newList }


replaceSame : Subject -> ( Int, Subject ) -> ( Int, Subject )
replaceSame new ( indice, orig ) =
    case orig.name == new.name of
        True ->
            ( indice, new )

        False ->
            ( indice, orig )


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
