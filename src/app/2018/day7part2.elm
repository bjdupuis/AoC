module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import Debug exposing (..)

type alias Line = 
    {dependency : String
    , step : String}

type alias Worker =
    { step : String
    , secondsRemaining : Int}

singleChar : Parser String
singleChar =
    getChompedString <|
        succeed identity
            |= chompIf Char.isAlpha


lineformat : Parser Line
lineformat =
    succeed Line
        |. token "Step "
        |= singleChar
        |. token " must be finished before step "
        |= singleChar

calculateAvailable : Dict String (List String) -> List String
calculateAvailable dependencies =
            let
                allPossibilities =
                    Dict.foldl
                        (\step deps set ->
                            Set.union (Set.fromList deps) set
                        ) Set.empty dependencies

            in  
                Dict.foldl
                    (\step deps set ->
                        case List.length deps of 
                            0 ->
                                set
                            _ ->
                                Set.remove step set
                    ) allPossibilities dependencies
                |> Set.toList


parseLines : List String -> Int
parseLines lines =
    let
        dependencyList = 
            List.map 
                (\line -> 
                    case Parser.run lineformat line of
                        Err err ->
                            let 
                                error = Debug.log "err" err

                            in
                                { step = "1", dependency = "1"}
                        Ok dependency ->
                            dependency
                ) lines

        dependencies =
            List.foldl 
                (\dependency dict ->
                    let 
                        stepDependencies =
                            case Dict.get dependency.step dict of
                                Nothing ->
                                    List.singleton dependency.dependency
                                Just xs ->
                                    dependency.dependency :: xs
                    in
                        Dict.insert dependency.step stepDependencies dict

                ) Dict.empty dependencyList

        available = 
            calculateAvailable dependencies

        workers = List.repeat 5 { step = "", secondsRemaining = 0 }

    in
        timeStep workers available dependencies 0

haveWorkersDoWork : List Worker -> List Worker
haveWorkersDoWork workers =
    List.map
        (\worker ->
            case String.length worker.step > 0 && worker.secondsRemaining > 0 of
                True ->
                    { worker | step = worker.step, secondsRemaining = worker.secondsRemaining - 1 }
                False ->
                    worker
        ) workers

stepToTime : String -> Int
stepToTime step =
    case String.uncons step of
        Just (char, rest) ->
            (Char.toCode char - Char.toCode 'A') + 1 + 60
        Nothing ->
            0

assignStepToAvailableWorker : String -> List Worker -> List Worker
assignStepToAvailableWorker step workers =
    if String.length step > 0 then
        case workers of 
            head :: rest ->
                if head.secondsRemaining == 0 then
                    { head | step = step, secondsRemaining = (stepToTime step)} :: rest
                else
                    assignStepToAvailableWorker step (List.append rest [head])
            [] ->
                workers
    else 
        workers

assignStepsToAvailableWorkers : List Worker -> List String -> (List Worker, List String)
assignStepsToAvailableWorkers workers steps =
    let
        hasAvailableWorkers =
            areWorkersAvailable workers

    in
        if hasAvailableWorkers && (not (List.isEmpty steps)) then
            let
                step = 
                    List.head steps |> Maybe.withDefault ""

                remainingSteps = 
                    List.tail steps |> Maybe.withDefault []

                updatedWorkers =
                    assignStepToAvailableWorker step workers

            in
                assignStepsToAvailableWorkers updatedWorkers remainingSteps
        else
            (workers, steps)

areWorkersAvailable : List Worker -> Bool
areWorkersAvailable workers =
    List.foldl
        (\worker found ->
            case worker.secondsRemaining of
                0 -> 
                    True
                _ ->
                    found
        ) False workers

timeStep : List Worker -> List String -> Dict String (List String) -> Int -> Int
timeStep workers available dependencies elapsedTime =
    let
        workingWorkers = 
            haveWorkersDoWork workers

        stepsCompleted = 
            List.foldl 
                (\value steps ->
                    case (not (String.isEmpty value.step)) && value.secondsRemaining == 0 of
                        True ->
                            value.step :: steps
                        False ->
                            steps
                ) [] workingWorkers

        workersAvailable = 
            areWorkersAvailable workingWorkers

        updatedUnblockedStepsAndDependencies = 
            List.foldl
                (\step updated ->
                    case Dict.get step dependencies of
                        Nothing ->
                            {updated | dependencies = Dict.insert step [] updated.dependencies}
                        Just xs ->
                            let 
                                remainingDependencies =
                                    List.filter
                                        (\value ->
                                            not (List.member value stepsCompleted)
                                        ) xs

                            in
                                case List.isEmpty remainingDependencies of 
                                    True ->
                                        {updated | unblockedSteps = step :: updated.unblockedSteps}
                                    False ->
                                        {updated | dependencies = Dict.insert step remainingDependencies updated.dependencies}

                ) {unblockedSteps = [], dependencies = Dict.empty} (Dict.keys dependencies)

        stepsUnblocked =
            updatedUnblockedStepsAndDependencies.unblockedSteps
            |> Set.fromList
            |> Set.union (Set.fromList available) 
            |> Set.toList
            |> List.sort

    in
        if (not (List.isEmpty stepsUnblocked) && workersAvailable) then
            let
                workersAndStepsTuple =
                    assignStepsToAvailableWorkers workingWorkers stepsUnblocked

                updatedWorkers = Tuple.first workersAndStepsTuple

                updatedSteps = Tuple.second workersAndStepsTuple

            in
                timeStep updatedWorkers updatedSteps updatedUnblockedStepsAndDependencies.dependencies (elapsedTime + 1)
        else
            let
                areWorkersWorking =
                    List.foldl 
                        (\worker found ->
                            if worker.secondsRemaining > 0 then
                                True
                            else
                                found
                        ) False workingWorkers

            in
                if areWorkersWorking then
                    timeStep workingWorkers stepsUnblocked updatedUnblockedStepsAndDependencies.dependencies (elapsedTime + 1)
                else
                    elapsedTime

calculate : String -> Int
calculate inputVal =
    let
        lines = String.lines inputVal

    in
        if List.isEmpty lines then
            0
        else
            parseLines lines

type alias Model =
    { result : String }

initialModel : Model
initialModel =
    { result = "" }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            { model | result = String.fromInt <| calculate newValue }

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| model.result ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

