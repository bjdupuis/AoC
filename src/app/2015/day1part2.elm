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

calculate : String -> Int
calculate inputVal =
    String.foldl
        (\value state ->
            let 
                newFloor = 
                    case value of
                        '(' ->
                            state.floor + 1
                        ')' ->
                            state.floor - 1
                        _ ->
                            state.floor
            in
                if newFloor == -1 && state.result == 0 then
                    { state | result = state.position }
                else 
                    { state | floor = newFloor, position = state.position + 1 }
        ) { floor = 0, position = 1, result = 0 } inputVal
        |> (\value -> value.result)

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

