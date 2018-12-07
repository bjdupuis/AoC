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

type alias Intersection =
    { distance : Int }

defaultIntersection : Intersection 
defaultIntersection = 
    { distance = 0 }

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
                            Just {xs | distance = 0}
                ) dict
                |> populateGrid (index + 1) remaining
        [] ->
            dict

updateDistance : Int -> Int -> Int -> Point -> Dict Point Intersection -> Dict Point Intersection
updateDistance x y index point dict =
    Dict.update (x, y) 
        (\intersection ->
            case intersection of
                Nothing ->
                    Just defaultIntersection
                Just xs ->
                    let
                        localDistance = calculateDistance x y point
                    
                    in
                       Just {distance = localDistance + xs.distance}
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
                                        updateDistance x y index head d2
                                    ) d1
                        ) dict
                    |> calculateDistances (index + 1) minPoint maxPoint remaining 
        [] -> 
            dict

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

filterDistantAreas : List Intersection -> Int
filterDistantAreas intersections =
    List.filter 
        (\value ->
            value.distance < 10000
        ) intersections
        |> Debug.log "sd"
        |> List.length

calculate : String -> Int
calculate inputVal =
    String.lines inputVal
        |> calculatePlane
        |> Dict.values
        |> filterDistantAreas

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

