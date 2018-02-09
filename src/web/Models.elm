module Models exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Decode exposing (..)
import Json.Decode.Pipeline


type Msg
    = NewList (Result Http.Error (List Subject))
    | ExpandSubject Subject
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
    { subjects : List Subject
    , loading : Bool
    , toasterMsg : String
    , tiredMode : Bool
    , apiEndpoint : String
    }


type alias DoneData =
    { description : String
    , whatToDoNext : String
    }


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , open : Bool
    , doneForm : Bool
    , doneData : DoneData
    , whatToDoNext : String
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


decodeSubjectList : Decoder (List Subject)
decodeSubjectList =
    Json.Decode.list decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodeStudyEntry) []
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.hardcoded (DoneData "" "")
        |> Json.Decode.Pipeline.required "what_to_do_next" (Json.Decode.string)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.list decodeStudyEntry)


decodeEmptyResult =
    Json.Decode.succeed ""
