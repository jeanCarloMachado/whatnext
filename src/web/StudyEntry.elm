module StudyEntry exposing (..)

import Json.Decode.Pipeline
import Json.Decode
import Html.Styled exposing (..)
import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import Colors exposing (defaultColors)


type alias Data =
    { date : String
    , description : String
    , subjectName : String
    }


decodeStudyEntry =
    Json.Decode.Pipeline.decode Data
        |> Json.Decode.Pipeline.required "date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)


toHtml studyEntry =
    li []
        [ p [ css [ color defaultColors.textHighlight ] ] [ text <| "Subject: " ++ studyEntry.subjectName ]
        , p [] [ text <| "Date: " ++ studyEntry.date ]
        , p [] [ text <| "Description: " ++ studyEntry.description ]
        ]
