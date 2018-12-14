module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Set exposing (Set)

countUpTo2DifferencesInStrings : String -> String -> Int -> List Int -> List Int
countUpTo2DifferencesInStrings first second index current = 
    let 
        leftOfFirst = String.left 1 first

        leftOfSecond = String.left 1 second

        log = Debug.log ("First " ++ first ++ " second " ++ second)
    in 
        if String.isEmpty leftOfFirst || String.isEmpty leftOfSecond then 
            current
        else
            if leftOfFirst == leftOfSecond then
                countUpTo2DifferencesInStrings (String.dropLeft 1 first) (String.dropLeft 1 second) (index + 1) current 
            else
                if List.length current == 1 then
                    index :: current 
                else 
                    countUpTo2DifferencesInStrings (String.dropLeft 1 first) (String.dropLeft 1 second) (index + 1) (index :: current)

parseList : String -> List String -> List String -> String
parseList current original rest =
    let 
        first = List.head rest |> Maybe.withDefault ""

        differences = countUpTo2DifferencesInStrings current first 0 []
    in 
        if List.length differences < 2 then
            String.append (String.slice 0 (List.head differences |> Maybe.withDefault 0) current ) (String.dropLeft ((List.head differences |> Maybe.withDefault 0) + 1) current)
        else
            if List.length rest == 1 then
                if List.length original == 1 then
                    "empty"
                else
                    parseList (List.head original |> Maybe.withDefault "") (List.tail original |> Maybe.withDefault []) (List.tail original |> Maybe.withDefault [])
            else
                parseList current original (List.tail rest |> Maybe.withDefault [])

calculate : String -> String
calculate inputVal =
    let
        lines = String.lines inputVal

        first = List.head lines |> Maybe.withDefault ""

        rest = List.tail lines |> Maybe.withDefault []

    in
        if List.length lines < 2 then
            "Input incorrect"
        else
            parseList first rest rest

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

