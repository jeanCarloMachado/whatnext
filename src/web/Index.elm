module Index exposing (..)

import Html
import Html.Styled
import Http exposing (..)
import Platform exposing (..)
import Json.Encode
import View exposing (..)
import Models exposing (..)


type alias Flags =
    { apiEndpoint : String }


emptySubject : Subject
emptySubject =
    Subject "" 0 "" [] False False (DoneData "" "") ""


main =
    Html.programWithFlags { init = init, view = view >> Html.Styled.toUnstyled, update = update, subscriptions = subscriptions }


init : Flags -> ( PageData, Cmd Msg )
init flags =
    ( PageData [] True "" False flags.apiEndpoint, getList flags.apiEndpoint False )


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        NewList (Ok subjects) ->
            ( { model | subjects = subjects, loading = False }, Cmd.none )

        ExpandSubject subject ->
            case subject.open of
                True ->
                    ( (replaceSubjectFromList model { subject | open = False }), Cmd.none )

                False ->
                    ( { model | loading = True }, getDetail model.apiEndpoint subject )

        GetDetail (Ok subject) ->
            let
                newModel =
                    (replaceSubjectFromList model { subject | open = True })
            in
                ( { newModel | loading = False }, Cmd.none )

        GetDetail (Err msg) ->
            ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )

        ClickDone subject ->
            ( (replaceSubjectFromList model { subject | doneForm = True, open = True }), Cmd.none )

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
            ( { model | loading = True }, doneRequest model.apiEndpoint subject )

        DoneResult (Ok _) ->
            ( { model | loading = False }, getList model.apiEndpoint model.tiredMode )

        DoneResult (Err msg) ->
            ( { model | toasterMsg = (toString msg), loading = False }, Cmd.none )

        ToggleTiredMode ->
            ( { model | tiredMode = not model.tiredMode }, getList model.apiEndpoint <| not model.tiredMode )

        RemoveClick subject ->
            ( { model | loading = True }, removeRequest model.apiEndpoint subject )

        Remove (Ok _) ->
            ( { model | loading = True }, getList model.apiEndpoint model.tiredMode )

        Remove (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        None ->
            ( model, Cmd.none )


getDetail : String -> Subject -> Cmd Msg
getDetail endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/detail/" ++ subject.name

        request =
            Http.get url decodeSubject
    in
        Http.send GetDetail request


doneRequest : String -> Subject -> Cmd Msg
doneRequest endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/done/" ++ subject.name

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string subject.doneData.description )
                , ( "whatToDoNext", Json.Encode.string subject.doneData.whatToDoNext )
                ]

        request =
            Http.post url (Http.jsonBody body) decodeEmptyResult
    in
        Http.send DoneResult request


removeRequest : String -> Subject -> Cmd Msg
removeRequest endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/rm/" ++ subject.name

        request =
            Http.get url decodeEmptyResult
    in
        Http.send Remove request


getList : String -> Bool -> Cmd Msg
getList endpoint tiredMode =
    let
        url =
            "http://" ++ endpoint ++ "/scheduler" ++ (tiredMode |> toUrlBool)

        request =
            Http.get url decodeSubjectList
    in
        Http.send NewList request


toUrlBool : Bool -> String
toUrlBool bool =
    case bool of
        True ->
            "?tiredMode=True"

        False ->
            ""



-- VIEW


replaceSubjectFromList : PageData -> Subject -> PageData
replaceSubjectFromList model subject =
    let
        newList =
            (List.map (\x -> replaceSame subject x) model.subjects)
    in
        { model | subjects = newList }


replaceSame : Subject -> Subject -> Subject
replaceSame new orig =
    case orig.name == new.name of
        True ->
            new

        False ->
            orig


subjectByName : String -> List Subject -> Subject
subjectByName subjectName subjectList =
    case List.filter (\x -> x.name == subjectName) subjectList of
        a :: _ ->
            a

        _ ->
            emptySubject



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
