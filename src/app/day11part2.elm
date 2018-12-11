module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
import Set exposing (..)
import Debug exposing (..)

showDebug = True

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> { highestTopLeft : ( Int, Int ), highestGridSize : Int }
calculate inputVal =
    let
        serialNumber =
            String.toInt inputVal |> Maybe.withDefault 0
    
        fuelGrid=
            List.foldl 
                (\x g ->
                    List.foldl
                        (\y grid ->
                            let 
                                rackId = x + 10

                                powerLevel = 
                                        String.fromInt (((rackId * y) + serialNumber) * rackId)
                                        |> String.dropRight 2
                                        |> String.right 1
                                        |> String.toInt
                                        |> Maybe.withDefault 0
                                        |> (\val -> val - 5) 
                            in
                                Dict.insert (x,y) powerLevel grid
                        ) g (List.range 1 300)
                ) Dict.empty (List.range 1 300)
--            |> maybeDebug "grid"
    in
        List.foldl
            (\gridSize result ->
                let
                    current =
                        calculatePowerLevelsForGrid gridSize fuelGrid result.currentGrid

                    newResult =
                        if current.highestPowerLevel > result.highestPowerLevel then 
                            { result
                            | highestPowerLevel = current.highestPowerLevel
                            , highestTopLeft = current.highestTopLeft
                            , highestGridSize = gridSize
                            , currentGrid = current.resultingGrid
                            }
                        else
                            { result 
                            | currentGrid = current.resultingGrid
                            }

                    debug = 
                        maybeDebug ("completed " ++ (String.fromInt gridSize)) { highestTopLeft = newResult.highestTopLeft, highestPowerLevel = newResult.highestPowerLevel }
                in
                    newResult

            ) { currentGrid = fuelGrid, highestGridSize = 1, highestTopLeft = (0,0), highestPowerLevel = 0} (List.range 2 300)
        |> (\result ->
                { highestTopLeft = result.highestTopLeft, highestGridSize = result.highestGridSize }
            )

calculatePowerLevelsForGrid : Int -> Dict (Int, Int) Int -> Dict (Int, Int) Int -> { resultingGrid : Dict (Int, Int) Int, highestPowerLevel : Int, highestTopLeft : (Int, Int)}
calculatePowerLevelsForGrid gridSize originalGrid grid =
    List.foldl 
        (\x total ->
            List.foldl
                (\y t ->
                    let 
                        powerLevel = 
                            calculatePowerLevelForTopLeft (x, y) gridSize originalGrid grid
                    
                        updatedGrid = 
                            Dict.insert (x,y) powerLevel t.resultingGrid

                    in
                        if powerLevel > t.highestPowerLevel then
                            { t 
                            | highestPowerLevel = powerLevel
                            , highestTopLeft = (x, y)
                            , resultingGrid = updatedGrid }
                        else
                            { t
                            | resultingGrid = updatedGrid }
                ) total (List.range 1 (300 - (gridSize - 1)))
        ) { highestPowerLevel = 0, highestTopLeft = (0,0), resultingGrid = Dict.empty } (List.range 1 (300 - (gridSize - 1)))


calculatePowerLevelForTopLeft : (Int, Int) -> Int -> Dict (Int, Int) Int -> Dict (Int, Int) Int -> Int
calculatePowerLevelForTopLeft topLeft gridSize originalGrid grid =
    let 
        startingValue =
            Dict.get topLeft grid |> Maybe.withDefault 0

    in
        List.foldl 
            (\x partial ->
                let
                    value = 
                        Dict.get (x, (Tuple.second topLeft) + (gridSize - 1)) originalGrid
                        |> Maybe.withDefault 0
                in
                    partial + value

            ) startingValue (List.range (Tuple.first topLeft) (Tuple.first topLeft + (gridSize - 1)))
        |> (\partial ->
                List.foldl 
                    (\y total ->
                        let
                            value = 
                                Dict.get ((Tuple.first topLeft) + (gridSize - 1), y) originalGrid
                                |> Maybe.withDefault 0
                        in
                            total + value

                    ) partial (List.range (Tuple.second topLeft) (Tuple.second topLeft + (gridSize - 2)))
            )

type alias Model =
    { topLeft : (Int, Int)
    , gridSize : Int }

initialModel : Model
initialModel =
    { topLeft = (0, 0)
    , gridSize = 0 }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            let
                result = calculate newValue
            in
                { model | topLeft = result.highestTopLeft, gridSize = result.highestGridSize }

view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [text <| (String.fromInt (Tuple.first model.topLeft)) ++ "," ++ (String.fromInt (Tuple.second model.topLeft)) ++ "," ++ (String.fromInt model.gridSize) ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

