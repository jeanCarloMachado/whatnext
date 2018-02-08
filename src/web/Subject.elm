module Subject exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import StudyEntry
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Global exposing (defaultColors)
import Http exposing (..)


type alias Subject =
    { name : String
    , daysSinceLast : Int
    , timeAlreadyInvested : String
    , history : List StudyEntry.Data
    , open : Bool
    , whatToDoNext : String
    }


type Msg
    = ExpandSubject Subject
    | GetDetail (Result Http.Error Subject)
    | None


update : Msg -> Subject -> ( Subject, Cmd Msg )
update msg model =
    case msg of
        ExpandSubject subject ->
            case subject.history of
                [] ->
                    ( model, getDetail model.apiEndpoint { subject | open = True } )

                _ ->
                    ( { subject | open = not subject.open }, Cmd.none )

        GetDetail (Ok subject) ->
            ( subject, Cmd.none )

        GetDetail (Err msg) ->
            ( model, Cmd.none )

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


emptySubject =
    Subject "" 0 "" [] False ""


decodeSubject : Decoder Subject
decodeSubject =
    Json.Decode.Pipeline.decode Subject
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "days_since_last_study" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "time_already_invested_str" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "history" (Json.Decode.list StudyEntry.decodeStudyEntry) []
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.required "what_to_do_next" (Json.Decode.string)


decodeSubjectHistory =
    at [ "history" ] (Json.Decode.list StudyEntry.decodeStudyEntry)


subjectByName subjectName subjectList =
    case List.filter (\x -> x.name == subjectName) subjectList of
        a :: _ ->
            a

        _ ->
            emptySubject


subjectToHtml subject =
    let
        historyHtml =
            subjectHistory subject

        nextStep =
            nextStepHtml subject
    in
        li [ onClick (ExpandSubject subject), subjectCss subject ]
            [ div []
                [ div [ css [ fontSize (Css.em 1.2) ] ]
                    [ p [ css [ color defaultColors.textHighlight ] ] [ text subject.name ]
                    , p [] [ text ((subject.daysSinceLast |> toString) ++ " days ago") ]
                    , p [] [ text subject.timeAlreadyInvested ]
                    ]
                , div
                    [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed None)
                    ]
                    [ div []
                        [ nextStep
                        ]
                    , historyHtml
                    ]
                ]
            ]


nextStepHtml subject =
    case subject.open of
        True ->
            div [ css [ fontSize (Css.em 1.1) ] ]
                [ text <| "What to do next: " ++ subject.whatToDoNext
                ]

        False ->
            div [] []


subjectHistory subject =
    case subject.open of
        True ->
            div []
                [ text "History"
                , div [] (List.map StudyEntry.toHtml subject.history)
                ]

        False ->
            div [] []


subjectCss subject =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), selectedColor subject |> backgroundColor ]


selectedColor subject =
    case subject.open of
        True ->
            defaultColors.selectedBackground

        _ ->
            defaultColors.normalBackground
