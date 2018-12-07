module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, token, oneOf, map)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import Debug exposing (..)
import List.Extra exposing (..)

parseString : String -> String -> String
parseString current remaining =
    let 
        left = String.right 1 current

        right = String.left 1 remaining

        destroy = left /= right && 
            ( String.toUpper left == String.toUpper right )

        newCurrent = case destroy of 
            True -> 
                if String.length current == 1 then
                    String.left 1 remaining
                else
                    String.dropRight 1 current
            
            False -> 
                current ++ right

        newRemaining = String.dropLeft 1 remaining

    in
        if String.isEmpty newRemaining then
            newCurrent
        else
            parseString newCurrent newRemaining

removeSequences : String -> Char -> String
removeSequences string char =
    let
        compare = Char.toUpper char

        filtered = String.filter (\val -> Char.toUpper val /= compare) string
    in
        parseString (String.left 1 filtered) (String.dropLeft 1 filtered)

processAndKeepShortest : String -> Char -> String -> String
processAndKeepShortest string charToRemove currentShortest =
    let 
        result = removeSequences string charToRemove

        log = Debug.log "result" result
    in
        if String.isEmpty currentShortest || String.length result < String.length currentShortest then
            result
        else
            currentShortest

calculate : String -> Int
calculate inputVal =
    let 
        chars = String.toList "abcdefghijklmnopqrstuvwxyz"

    in 
        List.foldl (processAndKeepShortest inputVal) "" chars
            |> String.length

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

