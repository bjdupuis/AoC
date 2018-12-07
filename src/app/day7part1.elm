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


parseLines : List String -> String
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

        log1 = Debug.log "dependencyList" dependencyList

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

        log2 = Debug.log "dependencies" dependencies

        available = 
            calculateAvailable dependencies

        log3 = Debug.log "available" available

    in
        processAvailable available dependencies ""

processAvailable : List String -> Dict String (List String) -> String -> String
processAvailable available dependencies result =
    case (List.sort available) of 
        head :: rest ->
            let
                updatedDependencies =
                    List.foldl
                        (\key dict ->
                            case Dict.get key dependencies of
                                Nothing ->
                                    Dict.remove key dict
                                Just xs ->
                                    let 
                                        remainingDependencies =
                                            List.filter 
                                                (\value ->
                                                    value /= head
                                                ) xs
                                    in
                                        if List.length remainingDependencies == 0 then
                                            Dict.remove key dict
                                        else 
                                            Dict.insert key remainingDependencies dict
                        ) Dict.empty (Dict.keys dependencies)

                log1 = Debug.log "updatedDependencies" updatedDependencies

                newlyAvailable =
                    List.filter 
                        (\key ->
                            Dict.member key dependencies && (not <| Dict.member key updatedDependencies)
                        ) (Dict.keys dependencies)
                        |> Set.fromList
                log3 = Debug.log "newlyAvailable" newlyAvailable

                updatedAvailable = Set.union 
                    (Set.fromList (calculateAvailable updatedDependencies)) newlyAvailable
                    |> Set.toList
                    
                log2 = Debug.log "updatedAvailable" updatedAvailable

            in
                processAvailable updatedAvailable updatedDependencies (result ++ head)

        [] ->
            result

calculate : String -> String
calculate inputVal =
    let
        lines = String.lines inputVal

    in
        if List.isEmpty lines then
            ""
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
            { model | result = calculate newValue }

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

