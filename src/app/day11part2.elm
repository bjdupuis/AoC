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

calculate : String -> Model
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
            (\gridSize best ->
                let
                    current = calculateHighestPowerLevelForGridSize gridSize fuelGrid

                in
                    if current.highestPowerLevel > best.highestPowerLevel then
                        {best | highestGridSize = gridSize, highestTopLeft = current.highestTopLeft, highestPowerLevel = current.highestPowerLevel}
                    else
                        best
            ) { highestTopLeft = (0,0), highestPowerLevel = 0, highestGridSize = 0 } (List.range 1 300)
        |> (\result ->
                { topLeft = result.highestTopLeft
                , size = result.highestGridSize}
            )

calculateHighestPowerLevelForGridSize : Int -> Dict (Int, Int) Int -> { highestTopLeft : (Int, Int), highestPowerLevel : Int }
calculateHighestPowerLevelForGridSize gridSize grid =
    List.foldl
        (\x result ->
            List.foldl
                (\y r ->
                    let
                        currentGridTopLeft = 
                            (x, y)

                        powerLevel = 
                            calculatePowerLevelForGrid gridSize currentGridTopLeft grid

                    in
                        if powerLevel > r.highestPowerLevel then
                            {r | highestPowerLevel = powerLevel, highestTopLeft = currentGridTopLeft}
                        else
                            r
                ) result (List.range 1 (300 - (gridSize - 1)))
        ) { highestTopLeft = (0,0), highestPowerLevel = 0 } (List.range 1 (300 - (gridSize - 1)))
            |> maybeDebug ("Finished " ++ (String.fromInt gridSize))

calculatePowerLevelForGrid : Int -> (Int, Int) -> Dict (Int, Int) Int -> Int
calculatePowerLevelForGrid gridSize topLeft grid =
    List.foldl 
        (\x total ->
            List.foldl
                (\y t ->
                    t + (Dict.get (x, y) grid |> Maybe.withDefault 0)
                ) total (List.range (Tuple.second topLeft) ((Tuple.second topLeft) + gridSize - 1))
        ) 0 (List.range (Tuple.first topLeft) ((Tuple.first topLeft) + (gridSize - 1)))

type alias Model =
    { topLeft : (Int, Int)
    , size : Int }

initialModel : Model
initialModel =
    { topLeft = (0, 0)
    , size = 0 }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            let
                result = calculate newValue

            in
                { model | topLeft = result.topLeft, size = result.size }

view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [text <| (String.fromInt (Tuple.first model.topLeft)) ++ "," ++ (String.fromInt (Tuple.second model.topLeft)) ++ "," ++ String.fromInt model.size ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

