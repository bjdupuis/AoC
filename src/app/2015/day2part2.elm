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

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

get : Int -> Array Int -> Int
get index array =
    Array.get index array |> Maybe.withDefault 0

calculate : String -> Int
calculate inputVal =
    let
        lines =
            String.lines inputVal
        
    in
        List.foldl
            (\line total ->
                let
                    dimensions =
                        String.split "x" line
                        |> List.map (\value -> String.toInt value |> Maybe.withDefault 0)
                        |> List.sort
                        |> Array.fromList

                    minimumFacePerimeter =
                        2 * get 0 dimensions + 2 * get 1 dimensions

                    cubicFeet =
                        get 0 dimensions  * get 1 dimensions * get 2 dimensions

                in
                    total + minimumFacePerimeter + cubicFeet
            ) 0 lines

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

