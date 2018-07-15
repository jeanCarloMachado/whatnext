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
    , children : List String
    , whatToDoNext : String
    , complexity : Int
    , priority : Int
    , objective : String
    , parent : String
    }

emptySubject : Subject
emptySubject = Subject
                ""
                0
                0
                []
                []
                ""
                50
                50
                ""
                ""


type alias SubjectIdentifier r =
    { r
        | name : String
    }


type alias PastAction =
    { date : String
    , description : String
    , subjectName : String
    , duration : Int
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
    }


type alias DoneInfo r =
    { r
        | name : String
        , description : String
        , whatToDoNext : String
        , duration : Int
    }

replaceSame : Subject -> ( Int, Subject ) -> ( Int, Subject )
replaceSame new ( indice, orig ) =
    case orig.name == new.name of
        True ->
            ( indice, new )

        False ->
            ( indice, orig )


--- requests


doneRequest : RequestMetadata r -> DoneInfo i -> Http.Request String
doneRequest requestMetadata doneInfo =
    let
        url =
            requestMetadata.apiEndpoint ++ "/done"

        body =
            Json.Encode.object
                [ ( "description", Json.Encode.string doneInfo.description )
                , ( "followup", Json.Encode.string doneInfo.whatToDoNext )
                , ( "subjectName", Json.Encode.string doneInfo.name )
                , ( "duration", Json.Encode.int doneInfo.duration )
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


removeRequest : RequestMetadata r -> SubjectIdentifier i -> Http.Request String
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


alterSubjectRequest : RequestMetadata r -> Subject -> String -> Http.Request String
alterSubjectRequest requestMetadata subject previousName =
    let
        url =
            requestMetadata.apiEndpoint ++ "/addOrUpdate"

        body = [ ( "name", Json.Encode.string subject.name )
            , ( "complexity", Json.Encode.int subject.complexity )
            , ( "priority", Json.Encode.int subject.priority )
            , ( "whatToDoNext", Json.Encode.string subject.whatToDoNext )
            , ( "objective", Json.Encode.string subject.objective )
            , ( "previousName", Json.Encode.string previousName)
            ] |> addParent subject
    in
        Http.request
            { method = "POST"
            , headers = defaultHeaders requestMetadata
            , url = url
            , body = (Http.jsonBody <| Json.Encode.object body)
            , expect = (Http.expectJson decodeEmptyResult)
            , timeout = Nothing
            , withCredentials = False
            }


addParent subject data =
    if String.isEmpty subject.parent
    then
        data
    else
        data ++ [("parent", Json.Encode.string subject.parent)]



defaultHeaders requestMetadata =
    [ Http.header "Content-Type" "application/json", Http.header "Authorization" requestMetadata.authToken ]



decodeEmptyResult =
    Json.Decode.succeed ""




getListRequest : RequestMetadata r -> Bool -> Http.Request (List Subject)
getListRequest state tiredMode =
    let
        url =
            state.apiEndpoint ++ "/scheduler" ++ (tiredMode |> toUrlBool)
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


--- default values


--- setters

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
setParent subject parent =
  {subject | parent = parent }

--decoders


decodeSubjectList : Decoder (List Subject)
decodeSubjectList =
    Json.Decode.list decodeSubject


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" Json.Decode.int
        |> Json.Decode.Pipeline.required "time_already_invested" Json.Decode.int
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list decodePastAction) []
        |> Json.Decode.Pipeline.optional "children" (Json.Decode.list Json.Decode.string) []
        |> Json.Decode.Pipeline.required "whatToDoNext" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "complexity" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "priority" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "objective" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "parent" (Json.Decode.string) ""

decodeSubjectHistory =
    at [ "history" ] (Json.Decode.array decodePastAction)


decodePastAction =
    Json.Decode.Pipeline.decode PastAction
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "duration" (Json.Decode.int)



priorities : List (String, String)
priorities =
    [ ( "0", "No Priority" )
    , ( "20", "Low" )
    , ( "50", "Medium" )
    , ( "70", "High" )
    , ( "100", "Higest" )
    ]

getPriorityString priority =
    let
        filtered = List.filter (\x -> (Tuple.first x) == priority) priorities
    in
       Tuple.second <| Maybe.withDefault (priority, priority) <| List.head filtered

getComplexityString complexity =
    let
        filtered = List.filter (\x -> (Tuple.first x) == complexity) complexities
    in
       Tuple.second <| Maybe.withDefault (complexity, complexity) <| List.head filtered


complexities : List (String, String)
complexities =
    [ ( "10", "Easy" )
    , ( "50", "Medium" )
    , ( "80", "Hard" )
    , ( "100", "Hardest" )
    ]


