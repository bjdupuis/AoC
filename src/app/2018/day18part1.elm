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

showDebug = False

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
    in
        List.foldl
            (\index result ->
                calculateMap result
            ) lines (List.range 1 10)
        |> (\map ->
                let 
                    result =
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
                    {result = result.trees * result.lumberyards
                    , map = map}
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

