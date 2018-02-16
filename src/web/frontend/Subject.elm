module Subject exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


type alias DoneData r =
    { r
        | doneSubjectName : String
        , doneDescription : String
        , doneWhatToDoNext : String
    }



--- api


setCurrentDoneSubject : DoneData r -> String -> DoneData r
setCurrentDoneSubject doneData name =
    { doneData | doneSubjectName = name }



--decoders


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodeStudyEntry) []
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



--- requests


doneRequest endpoint doneData =
    let
        url =
            "https://" ++ endpoint ++ "/done/" ++ doneData.doneSubjectName

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string doneData.doneDescription )
                , ( "whatToDoNext", Json.Encode.string doneData.doneWhatToDoNext )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = url
            , body = (Http.jsonBody body)
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = True
            }


removeRequest endpoint subject =
    let
        url =
            "https://" ++ endpoint ++ "/rm/" ++ subject.name
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = url
            , body = Http.emptyBody
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = True
            }


getDetail endpoint subject =
    let
        url =
            "https://" ++ endpoint ++ "/detail/" ++ subject.name

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeSubject)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        request


decodeEmptyResult =
    Json.Decode.succeed ""
