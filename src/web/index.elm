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


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry
    , current : Bool
    }


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


type alias PageData =
    { subjects : List Subject
    , loading : Bool
    , toasterMsg : String
    }



-- Init


init : ( PageData, Cmd Msg )
init =
    ( PageData [] True "", getList )



-- UPDATE


type Msg
    = NewList (Result Http.Error (List Subject))
    | ExpandSubject
    | GetDetail (Result Http.Error (List StudyEntry))
    | Done


update : Msg -> PageData -> ( PageData, Cmd Msg )
update msg model =
    case msg of
        NewList (Err msg) ->
            ( PageData [] False (toString msg), Cmd.none )

        NewList (Ok subjects) ->
            ( PageData subjects False "", Cmd.none )

        ExpandSubject ->
            case List.head model.subjects of
                Just subject ->
                    ( model, getDetail subject )

                Nothing ->
                    ( model, Cmd.none )

        GetDetail (Ok subjectHistory) ->
            case model.subjects of
                a :: b ->
                    let
                        listHead =
                            { a | history = subjectHistory }

                        newHead =
                            { listHead | current = True }

                        newModel =
                            { model
                                | subjects = (newHead :: b)
                            }
                    in
                        ( newModel, Cmd.none )

                [] ->
                    ( model, Cmd.none )

        GetDetail (Err msg) ->
            ( { model | toasterMsg = (toString msg) }, Cmd.none )

        Done ->
            ( { model | loading = True }, Cmd.none )


getDetail : Subject -> Cmd Msg
getDetail subject =
    let
        url =
            "http://whatnext:5000/detail/" ++ subject.name

        request =
            Http.get url decodeSubjectHistory
    in
        Http.send GetDetail request


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.list decodeStudyEntry)


decodeStudyEntry =
    Json.Decode.Pipeline.decode StudyEntry
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


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
        |> Json.Decode.Pipeline.hardcoded []
        |> Json.Decode.Pipeline.hardcoded False



-- VIEW


studyEntryToHtml studyEntry =
    div [ css [ padding (px 5) ] ]
        [ text (studyEntry.date)
        , span []
            [ text (": " ++ studyEntry.description)
            ]
        ]


selectedColor subject =
    case subject.current of
        True ->
            hex "add8e6"

        _ ->
            hex "ffffff"


subjectToHtml subject =
    let
        detailHtml =
            List.map studyEntryToHtml subject.history

        color =
            selectedColor subject
    in
        li [ onClick ExpandSubject, css [ borderRadius (px 10), borderWidth (px 1), padding (px 20), marginBottom (px 1), backgroundColor color ] ]
            [ div []
                [ text
                    (subject.name ++ ":  " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested |> toString))
                , div []
                    detailHtml

                --
                ]
            ]


subjectsToHtml list =
    let
        innerList =
            List.map subjectToHtml list
    in
        ul [ css [ listStyle none ] ] innerList


doneMessage : PageData -> String
doneMessage pageData =
    case pageData.loading of
        True ->
            "Loading"

        False ->
            "Done"


getToasterHtml pageData =
    case String.length pageData.toasterMsg of
        0 ->
            div [] []

        _ ->
            div [ css [ borderStyle dashed, borderWidth (px 1), textAlign center ] ]
                [ text pageData.toasterMsg
                ]


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



-- SUBSCRIPTIONS


subscriptions : PageData -> Sub Msg
subscriptions model =
    Sub.none
