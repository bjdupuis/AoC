module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
import Array exposing (..)
import Set exposing (..)
import Grid exposing (..)
import Debug exposing (..)

showDebug = True

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> Model
calculate inputValue =
    let
        lines = 
            String.lines inputValue
            |> (\result ->
                    let
                        first =
                            List.head result |> Maybe.withDefault ""

                        filler =
                            String.repeat ((String.length first) + 2) " "

                        paddedLines = 
                            List.map
                                (\string ->
                                    String.pad (String.length first + 2) ' ' string
                                ) result
                    in
                        [filler] ++ paddedLines ++ [filler]
                )

        answers = 
            List.foldl
                (\index result ->
                    let 
                        map =
                            calculateMap result.map
                        
                        count = 
                            List.foldl
                                (\line counts ->
                                    let
                                        lumberyards = 
                                            List.length (String.indexes "#" line)

                                        trees = 
                                            List.length (String.indexes "|" line)
                                    in
                                        { counts 
                                        | lumberyards = counts.lumberyards + lumberyards
                                        , trees = counts.trees + trees }
                                ) { lumberyards = 0, trees = 0} map
                    in
                        { result 
                        | map = map
                        , answers = result.answers ++ [count.lumberyards * count.trees]}

                ) { map = lines, answers = [], result = 0 } (List.range 1 600)
    in
        List.foldl
            (\checkIndex result ->
                let
                    tail =
                        List.head (List.drop ((List.length result.answers) - 1) result.answers) |> Maybe.withDefault 0

                    toCompare = 
                        List.head (List.drop (((List.length result.answers) - 1) - checkIndex) result.answers) |> Maybe.withDefault 0
                in
                    if result.finalAnswer == 0 then
                        if tail == toCompare then
                            let
                                finalAnswerIndex =
                                    checkIndex - (modBy checkIndex (1000000000 - 600))
                                    |> maybeDebug "final index"

                                debug =
                                    maybeDebug "cycleLength" checkIndex

                                finalAnswer = 
                                    List.head (List.drop (((List.length result.answers) - 1) - finalAnswerIndex) result.answers) |> Maybe.withDefault 0
                            in
                                { result
                                | result = finalAnswer}
                        else
                            result
                    else
                        result
            ) { finalAnswer = 0, map = answers.map, result = answers.result, answers = answers.answers } (List.range 1 50)
        |> (\result ->
                { map = result.map
                , result = result.result }
            )

calculateMap : List String -> List String
calculateMap map =
    List.Extra.indexedFoldl
        (\lineIndex line result ->
            if lineIndex == 0 || lineIndex == (List.length map - 1) then
                result ++ [line]
            else 
                List.foldl 
                    (\charIndex inner ->
                        let
                            previousLine = 
                                List.head (List.drop (lineIndex - 1) map)
                                    |> Maybe.withDefault ""

                            nextLine = 
                                List.head (List.drop (lineIndex + 1) map)
                                    |> Maybe.withDefault ""

                            neighbors = 
                                String.slice (charIndex - 1) (charIndex + 2) line
                                |> (\string ->
                                        (String.left 1 string) ++ (String.right 1 string)
                                    )

                            upper = 
                                String.slice (charIndex - 1) (charIndex + 2) previousLine

                            lower = 
                                String.slice (charIndex - 1) (charIndex + 2) nextLine

                            completeNeighbors = 
                                upper ++ lower ++ neighbors

                            current =
                                String.slice charIndex (charIndex + 1) line

                            newSpace =
                                case current of
                                    "." ->
                                        if List.length (String.indexes "|" completeNeighbors) >= 3 then
                                            "|"
                                        else 
                                            "."

                                    "|" ->
                                        if List.length (String.indexes "#" completeNeighbors) >= 3 then
                                            "#"
                                        else 
                                            "|"

                                    "#" ->
                                        if String.contains "|" completeNeighbors && String.contains "#" completeNeighbors then
                                            "#"
                                        else
                                            "."

                                    val ->
                                        val
                        in
                            inner ++ newSpace
                    ) " " (List.range 1 ((String.length line) - 1))
                |> (\string ->
                        result ++ [string ++ " "]   
                    )
        ) [] map

type alias Model =
    { result : Int
    , map : List String }

initialModel : Model
initialModel =
    { result = 0
    , map = [] }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            calculate newValue

view : Model -> Html Msg
view model =
    div []
        [ 
            div [] [text <| String.fromInt model.result]
            , pre [] [text <| String.join "\n" model.map ]
            , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

