module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Array exposing (..)
import Set exposing (..)
import Debug exposing (..)
import MD5 exposing (..)

showDebug = True

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> String
calculate inputVal =
    calculatePossibleHash inputVal 0

calculatePossibleHash : String -> Int -> String
calculatePossibleHash seed current =
    let 
        hash =
            MD5.hex (seed ++ (String.fromInt current))
    
    in
        if String.startsWith "00000" hash then
            String.fromInt current 
        else
            calculatePossibleHash seed (current + 1)

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
        [ 
          div [] [text <| model.result ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

