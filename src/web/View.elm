module View exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, type_)
import Html.Styled.Events exposing (..)
import Models exposing (..)
import Json.Decode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Array exposing (Array)


type alias Colors =
    { textHighlight : Css.Color
    , textNormal : Css.Color
    , selectedBackground : Css.Color
    , normalBackground : Css.Color
    }


defaultColors =
    Colors
        (Css.hex "b58900")
        (Css.hex "657b83")
        (Css.hex "fdf6e3")
        (Css.hex "ffffff")



{-
   solarizedMainColor=#002b36
   solarized_base03=#002b36
   solarized_base02=#073642
   solarized_base01=#586e75
   solarized_base00=#657b83
   solarized_base0=#839496
   solarized_base1=#93a1a1
   solarized_base2=#eee8d5
   solarized_base3=#fdf6e3
   solarized_yellow=#b58900
   solarized_orange=#cb4b16
   solarized_red=#dc322f
   solarized_magenta=#d33682
   solarized_violet=#6c71c4
   solarized_blue=#268bd2
   solarized_cyan=#2aa198
   solarized_green=#859900
-}


view : PageData -> Html.Styled.Html Msg
view pageData =
    let
        loadingHtml =
            getLoadingHtml pageData.loading
    in
        div [ css [ color defaultColors.textNormal, top (px 0), left (px 0), margin (px 0), height (pct 100) ] ]
            [ loadingHtml
            , div [ css [ margin (pct 3) ] ]
                [ div []
                    [ input [ type_ "checkbox", onClick ToggleTiredMode ] []
                    , text "Tired mode"
                    ]
                , a [ css [ padding (px 10) ], href "index.html?env=development&page=log" ]
                    [ text "Log"
                    ]
                , a [ css [ padding (px 10) ], href "index.html?page=add" ]
                    [ text "Add"
                    ]
                , getToasterHtml pageData
                , div
                    []
                    [ subjectsToHtml pageData.openedIndex pageData.subjects
                    ]
                ]
            ]


subjectsToHtml : Maybe Int -> List ( Int, Subject ) -> Html.Styled.Html Msg
subjectsToHtml openedIndex list =
    let
        innerList =
            List.map (subjectToHtml openedIndex) list
    in
        ul [ css [ listStyle none ] ] innerList


subjectToHtml : Maybe Int -> ( Int, Subject ) -> Html.Styled.Html Msg
subjectToHtml openedIndice ( indice, subject ) =
    li [ onClick (ExpandSubjectClick ( indice, subject )), subjectCss openedIndice ( indice, subject ) ]
        [ div []
            [ div [ css [ fontSize (Css.em 1.2) ] ]
                [ span [ css [ color defaultColors.textHighlight ] ] [ text subject.name ]
                , text
                    (" " ++ (subject.daysSinceLast |> toString) ++ " days ago -  " ++ (subject.timeAlreadyInvested))
                , (doneControlButtons subject)
                ]
            , (hiddenSubjectHtml openedIndice ( indice, subject ))
            ]
        ]


hiddenSubjectHtml : Maybe Int -> ( Int, Subject ) -> Html.Styled.Html Msg
hiddenSubjectHtml openedIndice ( indice, subject ) =
    case openedIndice of
        Just openedIndiceValue ->
            if openedIndiceValue == indice then
                div [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed None) ]
                    [ div []
                        [ (doneFormForSubject subject)
                        , div [ css [ fontSize (Css.em 1.1) ] ]
                            [ text <| "What to do next: " ++ subject.whatToDoNext
                            ]
                        ]
                    , div []
                        [ text "History"
                        , div [] (List.map studyEntryToHtml subject.history)
                        ]
                    , subjectButton "Remove" (RemoveClick subject)
                    ]
            else
                emptyNode

        Nothing ->
            emptyNode


doneControlButtons : Subject -> Html.Styled.Html Msg
doneControlButtons subject =
    case subject.doneForm of
        True ->
            div [ css [ Css.float right ] ]
                [ subjectButton "Cancel" (CancelDone subject)
                , subjectButton "Confirm" (SubmitDone subject)
                ]

        False ->
            div [ css [ Css.float right ] ]
                [ subjectButton "Done" (ClickDone subject)
                ]


subjectButton : String -> Msg -> Html.Styled.Html Msg
subjectButton textStr msg =
    button [ onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed msg) ]
        [ text textStr ]


inputCss =
    css [ display block, width (px 300), margin (px 5), padding (px 10) ]


doneFormForSubject subject =
    case subject.doneForm of
        True ->
            div [ css [ paddingTop (px 10) ] ]
                [ input [ inputCss, type_ "text", placeholder "What was done?", onInput (DoneChangeDescription subject) ] []
                , input [ inputCss, type_ "text", placeholder "What is to de done next?", onInput (DoneChangeWhatToDoNext subject) ] []
                ]

        False ->
            div []
                []


subjectCss selectedIndex ( index, subject ) =
    css
        [ borderRadius (px 10), display block, borderWidth (px 1), padding (px 20), marginBottom (px 1), backgroundColor (selectedColor selectedIndex ( index, subject )) ]


selectedColor selectedIndex ( index, subject ) =
    case selectedIndex of
        Just x ->
            if index == x then
                defaultColors.selectedBackground
            else
                defaultColors.normalBackground

        _ ->
            defaultColors.normalBackground


getToasterHtml pageData =
    case String.length pageData.toasterMsg of
        0 ->
            div [] []

        _ ->
            div [ css [ borderStyle dashed, borderWidth (px 1), textAlign center ] ]
                [ text pageData.toasterMsg
                ]


studyEntryToHtml studyEntry =
    li []
        [ p [ css [ color defaultColors.textHighlight ] ] [ text <| "Subject: " ++ studyEntry.subjectName ]
        , p [] [ text <| "Date: " ++ studyEntry.date ]
        , p [] [ text <| "  " ++ studyEntry.description ]
        ]


emptyNode =
    text ""


getLoadingHtml enabled =
    case enabled of
        True ->
            div [ css [ justifyContent center, alignItems center, position fixed, displayFlex, top (px 0), left (px 0), width (pct 100), height (pct 100), backgroundColor <| rgba 255 255 255 0.9 ] ]
                [ text "Loading"
                ]

        False ->
            emptyNode
