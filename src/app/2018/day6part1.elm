module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, token, oneOf, map)
import Basics exposing (abs)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import List.Extra exposing (..)
import Debug exposing (..)

type alias Point = (Int, Int)

type Owner = Single Int | Multiple (Set Int) | NoOne

type alias Intersection =
    { owner : Owner 
    , distance : Int }

defaultIntersection : Intersection 
defaultIntersection = 
    { owner = NoOne
    , distance = 0 }

pointParser : Parser Point
pointParser =
    succeed Tuple.pair
        |= int
        |. token ", "
        |= int

parseVertex : String -> List Point -> List Point
parseVertex inputVal list =
    let
        point = case Parser.run pointParser inputVal of
            Ok aPoint ->
                aPoint
            Err err ->
                (0,0)

    in
        point :: list

createGrid : Point -> Point -> Dict Point Intersection
createGrid min size =
    let 
        dict = Dict.empty

        point = (0,0)
    in
        List.range ((Tuple.first min) - 1) (Tuple.first size)
        |> List.foldl 
            (\xs d1 -> 
                List.range ((Tuple.second min) - 1) (Tuple.second size)
                    |> List.foldl 
                        (\ys d2 -> 
                            Dict.insert (xs, ys) defaultIntersection d2
                        ) 
                    d1
            ) dict

calculateSize : List Point -> Point
calculateSize list =
    List.foldl 
        (\point current -> 
            (Basics.max (Tuple.first point) (Tuple.first current), Basics.max (Tuple.second point) (Tuple.second current))
        ) (0,0) list

calculateMinSize : List Point -> Point -> Point
calculateMinSize list max =
    List.foldl 
        (\point current -> 
            (Basics.min (Tuple.first point) (Tuple.first current), Basics.min (Tuple.second point) (Tuple.second current))
        ) max list

createAppropriateGrid : Point -> Point -> List Point -> Dict Point Intersection
createAppropriateGrid minPoint maxPoint list =
        createGrid minPoint maxPoint

populateGrid : Int -> List Point -> Dict Point Intersection -> Dict Point Intersection
populateGrid index points dict =
    case points of 
        head :: remaining ->
            Dict.update head 
                (\intersection -> 
                    case intersection of 
                        Nothing ->
                            Just defaultIntersection
                        Just xs -> 
                            Just {xs | owner = Single index, distance = -1}
                ) dict
                |> populateGrid (index + 1) remaining
        [] ->
            dict

updateOwnership : Int -> Int -> Int -> Point -> Dict Point Intersection -> Dict Point Intersection
updateOwnership x y index point dict =
    Dict.update (x, y) 
        (\intersection ->
            case intersection of
                Nothing ->
                    Just defaultIntersection
                Just xs ->
                    let
                        localDistance = calculateDistance x y point
                    
                    in
                        case xs.owner of
                            NoOne -> 
                                Just {owner = Single index, distance = localDistance}

                            Multiple owners ->
                                if xs.distance > localDistance then
                                    Just {owner = Single index, distance = localDistance}
                                else if xs.distance == localDistance then
                                    Just {owner = Multiple (Set.insert index owners), distance = xs.distance}
                                else
                                    Just xs
                            
                            Single currentOwner ->
                                if xs.distance > localDistance then
                                    Just {owner = Single index, distance = localDistance}
                                else if xs.distance == localDistance then
                                    Just {owner = Multiple (Set.fromList [index, currentOwner]), distance = xs.distance}
                                else
                                    Just xs
        ) dict



calculateDistance : Int -> Int -> Point -> Int
calculateDistance x y point =
    abs (x - (Tuple.first point)) + abs (y - (Tuple.second point))

calculateDistances : Int -> Point -> Point -> List Point -> Dict Point Intersection -> Dict Point Intersection
calculateDistances index minPoint maxPoint points dict =
    case points of 
        head :: remaining ->
                List.range ((Tuple.first minPoint) - 1) (Tuple.first maxPoint)
                    |> List.foldl 
                        (\x d1 -> 
                            List.range ((Tuple.second minPoint) - 1) (Tuple.second maxPoint) 
                                |> List.foldl 
                                    (\y d2 ->
                                        updateOwnership x y index head d2
                                    ) d1
                        ) dict
                    |> calculateDistances (index + 1) minPoint maxPoint remaining 
        [] -> 
            dict

processInfiniteEntry : Int -> Int -> Dict Point Intersection -> Set Int -> Set Int
processInfiniteEntry x y dict currentSet =
    let
        infinite = 
            case (Dict.get (x, y) dict) of 
                Just xs ->
                    Just xs.owner
                Nothing -> 
                    Just NoOne

    in
        case infinite of
            Nothing ->
                currentSet
            Just NoOne ->
                currentSet
            Just (Single owner) ->
                Set.insert owner currentSet
            Just (Multiple owners) ->
                currentSet


filterInfiniteAreas : Point -> Point -> Dict Point Intersection -> Dict Point Intersection
filterInfiniteAreas minPoint maxPoint dict =
    let
        toRemove =
            List.range ((Tuple.first minPoint) - 1) (Tuple.first maxPoint)
                |> List.foldl 
                    (\x s1 ->
                        List.foldl 
                            (\y s2 ->
                                processInfiniteEntry x y dict s2
                            ) s1 [((Tuple.second minPoint) - 1), Tuple.second maxPoint]
                    ) Set.empty
                |> Set.union 
                    (List.range ((Tuple.second minPoint) - 1) (Tuple.second maxPoint) 
                        |> List.foldl
                            (\y s1 ->
                                List.foldl 
                                    (\x s2 ->
                                        processInfiniteEntry x y dict s2
                                    ) s1 [((Tuple.first minPoint) - 1), Tuple.first maxPoint]
                            ) Set.empty
                    )
    in
        Dict.filter 
            (\key value ->
                case value.owner of
                    NoOne ->
                        False
                    Single owner ->
                        if Set.member owner toRemove then
                            False
                        else
                            True
                    Multiple owners ->
                        False
            ) dict

calculatePlane : List String -> Dict Point Intersection
calculatePlane vertices =
    let
        points = List.foldl parseVertex [] vertices
        
        maxPoint = calculateSize points

        minPoint = calculateMinSize points maxPoint

    in
        points
            |> createAppropriateGrid minPoint maxPoint
            |> populateGrid 1 points
            |> calculateDistances 1 minPoint maxPoint points
            |> filterInfiniteAreas minPoint maxPoint

gatherAreas : List Int -> List Intersection -> List Int
gatherAreas existing intersections =
    case intersections of
        head :: rest ->
            let 
                headOwner =
                    case head.owner of
                        Single owner ->
                            owner
                        _ ->
                            -1

                onlyThisOwner =
                    List.filter 
                        (\value ->
                            case value.owner of
                                Single owner ->
                                    headOwner == owner
                                _ ->
                                    False
                        ) rest

                thisOwnerRemoved =
                    List.filter 
                        (\value ->
                            case value.owner of
                                Single owner ->
                                    headOwner /= owner
                                _ ->
                                    False
                        ) rest
            in
                gatherAreas (((List.length onlyThisOwner) + 1) :: existing) thisOwnerRemoved

        [] -> 
            existing

calculate : String -> Int
calculate inputVal =
    String.lines inputVal
        |> calculatePlane
        |> Dict.values
        |> gatherAreas []
        |> List.maximum |> Maybe.withDefault 0

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
        [ div [] [ text <| String.fromInt model.result ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

