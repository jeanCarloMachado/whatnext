module Subject exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline
import Array exposing (Array)


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    , objective : String
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


type alias NewSubjectData r =
    { r
        | newSubjectName : String
        , newPriority : Int
        , newComplexity : Int
        , newObjective : String
        , newWhatToDoNext : String
    }



--- api


setCurrentDoneSubject : DoneData r -> String -> DoneData r
setCurrentDoneSubject doneData name =
    { doneData | doneSubjectName = name }


replaceSame : Subject -> ( Int, Subject ) -> ( Int, Subject )
replaceSame new ( indice, orig ) =
    case orig.name == new.name of
        True ->
            ( indice, new )

        False ->
            ( indice, orig )



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
        |> Json.Decode.Pipeline.required "objective" (Json.Decode.string)


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


addSubjectRequest : String -> NewSubjectData r -> Http.Request String
addSubjectRequest endpoint state =
    let
        url =
            "https://" ++ endpoint ++ "/addOrUpdate"

        body =
            Json.Encode.object
                [ ( "name", Json.Encode.string state.newSubjectName )
                , ( "complexity", Json.Encode.int state.newComplexity )
                , ( "priority", Json.Encode.int state.newPriority )
                , ( "whatToDoNext", Json.Encode.string state.newWhatToDoNext )
                , ( "objective", Json.Encode.string state.newObjective )
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


decodeEmptyResult =
    Json.Decode.succeed ""


replaceSubjectFromList : List ( Int, Subject ) -> Subject -> List ( Int, Subject )
replaceSubjectFromList list subject =
    (List.map (\x -> replaceSame subject x) list)


decodeSubjectList : Decoder (Array Subject)
decodeSubjectList =
    Json.Decode.array decodeSubject


getListRequest state =
    let
        url =
            "https://" ++ state.apiEndpoint ++ "/scheduler" ++ (state.tiredMode |> toUrlBool)
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = url
            , body = Http.emptyBody
            , expect = (Http.expectJson decodeSubjectList)
            , timeout = Nothing
            , withCredentials = True
            }


toUrlBool : Bool -> String
toUrlBool bool =
    case bool of
        True ->
            "?tiredMode=True"

        False ->
            ""
