module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Http exposing (..)
import Platform exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


main =
    Html.program { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }



--Model


type alias PageData =
    { subjects : List Subject
    , loading : Bool
    , toasterMsg : String
    }


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , open : Bool
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }



-- Init


init : ( PageData, Cmd Msg )
init =
    ( PageData [] True "", getList )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))
    | ExpandSubject Subject
    | GetDetail (Result Http.Error Subject)
    | Done


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( PageData [] False (toString msg), Cmd.none )

        NewList (Ok subjects) ->
            ( PageData subjects False "", Cmd.none )

        ExpandSubject subject ->
            case subject.open of
                False ->
                    ( model, getDetail subject )

                True ->
                    ( { model | subjects = (List.map (\x -> replaceSame { subject | open = False } x) model.subjects) }, Cmd.none )

        GetDetail (Ok subject) ->
            let
                newSubjects =
                    (List.map (\x -> replaceSame { subject | open = True } x) model.subjects)
            in
                ( { model | subjects = newSubjects }, Cmd.none )

        GetDetail (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        Done ->
            ( { model | loading = True }, Cmd.none )


replaceSame orig cmp =
    case orig.name == cmp.name of
        True ->
            orig

        False ->
            cmp


getDetail : Subject -> Cmd Msg
getDetail subject =
    let
        url =
            "http://whatnext:5000/detail/" ++ subject.name

        request =
            Http.get url decodeSubject
    in
        Http.send GetDetail request


getList : Cmd Msg
getList =
    let
        url =
            "http://whatnext:5000"

        request =
            Http.get url decodeSubjectList
    in
        Http.send NewList request


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


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.list decodeStudyEntry)


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)



-- VIEW


view : PageData -> Html.Styled.Html Msg
view pageData =
    div [ css [ position relative, top (px 0), left (px 0), margin (px 0), height (pct 100) ] ]
        [ div [ css [ margin (pct 3) ] ]
            [ getToasterHtml pageData
            , div
                []
                [ subjectsToHtml pageData.subjects
                ]
            ]
        ]


subjectsToHtml list =
    let
        innerList =
            List.map subjectToHtml list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml subject =
    let
        historyHtml =
            subjectHistory subject
    in
        li [ onClick (ExpandSubject subject), subjectCss subject ]
            [ div []
                [ text
                    (subject.name ++ ":  " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested |> toString))
                , div []
                    historyHtml

                --
                ]
            ]


subjectHistory subject =
    case subject.open of
        True ->
            List.map studyEntryToHtml subject.history

        False ->
            []


subjectCss subject =
    css
        [ borderRadius (px 10), borderWidth (px 1), padding (px 20), marginBottom (px 1), selectedColor subject |> backgroundColor ]


selectedColor subject =
    case subject.open of
        True ->
            hex "add8e6"

        _ ->
            hex "ffffff"


studyEntryToHtml studyEntry =
    div [ css [ padding (px 5) ] ]
        [ text (studyEntry.date)
        , span []
            [ text (": " ++ studyEntry.description)
            ]
        ]


getToasterHtml pageData =
    case String.length pageData.toasterMsg of
        0 ->
            div [] []

        _ ->
            div [ css [ borderStyle dashed, borderWidth (px 1), textAlign center ] ]
                [ text pageData.toasterMsg
                ]



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
