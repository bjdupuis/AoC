module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Array exposing (..)
import Set exposing (..)
import Debug exposing (..)

showDebug = True

initialStatePrecursor = "initial state: "

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> Int
calculate inputVal =
    parseInput inputVal

parseInput : String -> Int
parseInput inputVal =
    let
        lines = 
            String.lines inputVal

        initialState = 
            case List.head lines of 
                Nothing ->
                    ""

                Just line ->
                    if String.startsWith initialStatePrecursor line then
                        ".." ++ (String.dropLeft (String.length initialStatePrecursor) line) ++ ".."
                    else
                        ""

        -- This tree has entrees for all cases that would result in a plant being alive in the timeframe
        lifeTree =
            case lines of
                _ :: _ :: rest ->
                    List.foldl 
                        (\line tree ->
                            if String.endsWith "#" line then
                                insert (String.left 5 line) tree
                            else 
                                tree
                        ) Empty rest
                [_] ->
                    Empty

                [] ->
                    Empty
        lifetreeDebug =
            maybeDebug "lifeTree" lifeTree
    
    in
        List.foldl 
            (\generationNumber currentState ->
                let
                    debugGen =
                        case modBy 5000 generationNumber of
                            0 ->
                                maybeDebug "generation number " generationNumber
                            _ ->
                                0

                    
                    nextGenState = 
                        String.foldl
                            (\plant inner ->
                                let
                                    plantString = 
                                        String.fromChar plant

                                    nextGen =
                                        case willCurrentPlantByAliveNextGeneration inner.preceding plantString inner.remaining lifeTree of
                                            True ->
                                                '#'
                                            False ->
                                                '.'

                                in
                                    { inner | preceding = inner.preceding ++ plantString, remaining = (String.dropLeft 1 inner.remaining), generation = inner.generation ++ (String.fromChar nextGen) }

                            ) { preceding = "", remaining = (String.dropLeft 1 currentState.plants), generation = "" } currentState.plants

                    nextGeneration =
                        if String.contains "#" (String.left 2 nextGenState.generation) then
                            { plants = "." ++ nextGenState.generation, newPlantsAdded = currentState.plantsAdded + 1 }
                        else
                            { plants = nextGenState.generation, newPlantsAdded = currentState.plantsAdded }
                            |> (\state ->
                                    if String.contains "#" (String.right 2 state.plants) then
                                        { state | plants = state.plants ++ "." }
                                    else
                                        state
                                )


                    logNext =
                        maybeDebug "next" nextGeneration

                in
                    { currentState | plants = nextGeneration.plants, plantsAdded = nextGeneration.newPlantsAdded}

            ) { plants = initialState, plantsAdded = 2 } (List.range 1 201)
        |> (\state ->
                String.foldl
                    (\plant inner ->
                        let
                            result = 
                                case plant of
                                    '#' ->
                                        { inner | sum = inner.sum + inner.plantIndex }
                                    _ ->
                                        inner
                        in
                            { inner | sum = result.sum, plantIndex = inner.plantIndex + 1 }
                    ) { plantIndex = -state.plantsAdded, sum = 0 } state.plants
            )
        |> (\result ->
                result.sum
            )

willCurrentPlantByAliveNextGeneration : String -> String -> String -> Tree String -> Bool
willCurrentPlantByAliveNextGeneration before current after tree =
    let
        prefix =
            case String.length before of 
                0 ->
                    ".."
                1 ->
                    "." ++ before
                _ ->
                    String.right 2 before

        suffix = 
            case String.length after of
                0 ->
                    ".."
                1 ->
                    after ++ "."
                _ ->
                    String.left 2 after

        complete =
            prefix ++ current ++ suffix

    in
        contains complete tree

type alias Input =
    { initialState : String
    , lifeTree : Tree String }

type Tree a 
    = Empty
    | Node a (Tree a) (Tree a)

insert : comparable -> Tree comparable -> Tree comparable
insert item tree =
    case tree of
        Empty ->
            Node item Empty Empty

        Node val left right ->
            if item < val then
                Node val (insert item left) right
            else if item > val then
                Node val left (insert item right)
            else
                tree

contains : comparable -> Tree comparable -> Bool
contains item tree =
    case tree of 
        Empty ->
            False

        Node val left right ->
            if val == item then
                True
            else if item < val then
                contains item left
            else 
                contains item right

type alias Model =
    { result : Int }

initialModel : Model
initialModel =
    { result = 0 }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            { model | result = calculate newValue }

view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [text <| String.fromInt model.result ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

