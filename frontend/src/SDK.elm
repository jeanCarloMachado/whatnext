module SDK exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline
import Array exposing (Array)


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : Int
    , history : List PastAction
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    , objective : String
    }

emptySubject : Subject
emptySubject = Subject
                ""
                0
                0
                []
                ""
                50
                50
                ""

setName subject name =
  {subject | name = name }
setComplexity subject complexity =
  {subject | complexity = complexity }
setPriority subject priority =
  {subject | priority = priority }
setObjective subject objective =
  {subject | objective = objective }
setWhatToDoNext subject whatToDoNext =
  {subject | whatToDoNext = whatToDoNext }


type alias PastAction =
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

type alias RequestMetadata r =
    { r
        | apiEndpoint : String
        , authToken : String
    }


type alias SubjectData r =
    { r
        | newSubjectName : String
        , newPriority : Int
        , newComplexity : Int
        , newObjective : String
        , newWhatToDoNext : String
        , apiEndpoint : String
        , authToken : String
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
        |> Json.Decode.Pipeline.required "days_since_last_study" Json.Decode.int
        |> Json.Decode.Pipeline.required "time_already_invested" Json.Decode.int
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodePastAction) []
        |> Json.Decode.Pipeline.required "whatToDoNext" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "complexity" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "priority" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "objective" (Json.Decode.string)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.array decodePastAction)


decodePastAction =
    Json.Decode.Pipeline.decode PastAction
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)



--- requests


doneRequest requestMetadata doneData =
    let
        url =
            requestMetadata.apiEndpoint ++ "/done/" ++ doneData.doneSubjectName

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string doneData.doneDescription )
                , ( "followup", Json.Encode.string doneData.doneWhatToDoNext )
                ]
    in
        Http.request
            { method = "POST"
            , headers = defaultHeaders requestMetadata
            , url = url
            , body = (Http.jsonBody body)
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = False
            }


removeRequest requestMetadata subject =
    let
        url =
            requestMetadata.apiEndpoint ++ "/rm/" ++ subject.name
    in
        Http.request
            { method = "GET"
            , headers = defaultHeaders requestMetadata
            , url = url
            , body = Http.emptyBody
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = False
            }


getDetail : RequestMetadata r  -> String -> Http.Request Subject
getDetail requestMetadata subjectName =
    let
        url =
            requestMetadata.apiEndpoint ++ "/detail/" ++ subjectName

        request =
            Http.request
                { method = "GET"
                , headers = defaultHeaders requestMetadata
                , url = url
                , body = Http.emptyBody
                , expect = (Http.expectJson decodeSubject)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        request


addSubjectRequest : RequestMetadata r -> Subject -> Http.Request String
addSubjectRequest requestMetadata subjectData =
    let
        url =
            requestMetadata.apiEndpoint ++ "/addOrUpdate"

        body =
            Json.Encode.object
                [ ( "name", Json.Encode.string subjectData.name )
                , ( "complexity", Json.Encode.int subjectData.complexity )
                , ( "priority", Json.Encode.int subjectData.priority )
                , ( "whatToDoNext", Json.Encode.string subjectData.whatToDoNext )
                , ( "objective", Json.Encode.string subjectData.objective )
                , ( "previousName", Json.Encode.string "")
                ]
    in
        Http.request
            { method = "POST"
            , headers = defaultHeaders requestMetadata
            , url = url
            , body = (Http.jsonBody body)
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = False
            }

defaultHeaders requestMetadata =
    [ Http.header "Content-Type" "application/json", Http.header "Authorization" requestMetadata.authToken ]



decodeEmptyResult =
    Json.Decode.succeed ""


decodeSubjectList : Decoder (Array Subject)
decodeSubjectList =
    Json.Decode.array decodeSubject


getListRequest state =
    let
        url =
            state.apiEndpoint ++ "/scheduler" ++ (state.tiredMode |> toUrlBool)
    in
        Http.request
            { method = "GET"
            , headers = defaultHeaders state
            , url = url
            , body = Http.emptyBody
            , expect = (Http.expectJson decodeSubjectList)
            , timeout = Nothing
            , withCredentials = False
            }


toUrlBool : Bool -> String
toUrlBool bool =
    case bool of
        True ->
            "?tiredMode=True"

        False ->
            ""


getHistory state =
    let
        url =
            state.apiEndpoint ++ "/log"
    in
          Http.request
              { method = "GET"
              , headers = defaultHeaders state
              , url = url
              , body = Http.emptyBody
              , expect = (Http.expectJson decodeHistory)
              , timeout = Nothing
              , withCredentials = False
              }


decodeHistory =
    Json.Decode.array decodePastAction


errorResult state msg =
    ( { state | toasterMsg = (toString msg), loading = False }, Cmd.none )

