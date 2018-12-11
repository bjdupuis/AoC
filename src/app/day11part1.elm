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

showDebug = False

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> ( Int, Int )
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
            |> maybeDebug "grid"
    in
        List.foldl
            (\x result ->
                List.foldl
                    (\y r ->
                        let
                            currentGridTopLeft = 
                                (x, y)

                            powerLevel = 
                                calculatePowerLevelFor3x3 currentGridTopLeft fuelGrid

                        in
                            if powerLevel > r.highestPowerLevel then
                                {r | highestPowerLevel = powerLevel, highestTopLeft = currentGridTopLeft}
                            else
                                r
                    ) result (List.range 1 298)
            ) { highestTopLeft = (0,0), highestPowerLevel = 0 } (List.range 1 298)
        |> (\result -> result.highestTopLeft)

calculatePowerLevelFor3x3 : (Int, Int) -> Dict (Int, Int) Int -> Int
calculatePowerLevelFor3x3 topLeft grid =
    List.foldl 
        (\x total ->
            List.foldl
                (\y t ->
                    t + (Dict.get (x, y) grid |> Maybe.withDefault 0)
                ) total (List.range (Tuple.second topLeft) ((Tuple.second topLeft) + 2))
        ) 0 (List.range (Tuple.first topLeft) ((Tuple.first topLeft) + 2))

type alias Model =
    { topLeft : (Int, Int) }

initialModel : Model
initialModel =
    { topLeft = (0, 0) }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            { model | topLeft = calculate newValue }

view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [text <| "(" ++ (String.fromInt (Tuple.first model.topLeft)) ++ "," ++ (String.fromInt (Tuple.second model.topLeft)) ++ ")" ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

