module Entities exposing (..)


type alias PageData =
    { subjects : List Subject
    , loading : Bool
    , toasterMsg : String
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
    }


emptySubject =
    Subject "" 0 "" [] False False (DoneData "" "")


type alias StudyEntry =
    { date : String
    , description : String
    , subjectName : String
    }


replaceSubjectFromList model subject =
    let
        newList =
            (List.map (\x -> replaceSame subject x) model.subjects)
    in
        { model | subjects = newList }


replaceSame new orig =
    case orig.name == new.name of
        True ->
            new

        False ->
            orig


subjectByName subjectName subjectList =
    case List.filter (\x -> x.name == subjectName) subjectList of
        a :: _ ->
            a

        _ ->
            emptySubject
