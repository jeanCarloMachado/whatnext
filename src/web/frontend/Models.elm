module Models exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline
import Array exposing (Array)


type Msg
    = NewList (Result Http.Error (Array Subject))
    | ExpandSubjectClick ( Int, Subject )
    | GetDetail (Result Http.Error Subject)
    | DoneResult (Result Http.Error String)
    | SubmitDone Subject
    | ToggleTiredMode
    | ClickDone Subject
    | CancelDone Subject
    | DoneChangeDescription Subject String
    | DoneChangeWhatToDoNext Subject String
    | Remove (Result Http.Error String)
    | RemoveClick Subject
    | None


type alias PageData =
    { subjects : List ( Int, Subject )
    , openedIndex : Maybe Int
    , loading : Bool
    , toasterMsg : String
    , tiredMode : Bool
    , apiEndpoint : String
    }


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , doneForm : Bool
    , doneData : DoneData
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    }


type alias DoneData =
    { description : String
    , whatToDoNext : String
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


emptySubject : Subject
emptySubject =
    Subject "" 0 "" [] False (DoneData "" "") "" 0 0


decodeSubjectList : Decoder (Array Subject)
decodeSubjectList =
    Json.Decode.array decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodeStudyEntry) []
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded (DoneData "" "")
        |> Json.Decode.Pipeline.required "what_to_do_next" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "complexity" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "priority" (Json.Decode.int)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.array decodeStudyEntry)


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


decodeEmptyResult =
    Json.Decode.succeed ""


removeRequest : String -> Subject -> Cmd Msg
removeRequest endpoint subject =
    let
        url =
            "http://" ++ endpoint ++ "/rm/" ++ subject.name

        request =
            Http.get url decodeEmptyResult
    in
        Http.send Remove request


getListRequest : String -> Bool -> Cmd Msg
getListRequest endpoint tiredMode =
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
