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

type Which =
    Santa | Robo

calculate : String -> Int
calculate inputVal =
    let
        defaultGrid = 
            Dict.singleton (0, 0) 2
    in
    
    String.foldl 
        (\direction state ->
            let 
                updatedSantaCoordinates = 
                    case direction of
                        '^' ->
                            { x = state.location.x, y = state.location.y + 1 }

                        'v' ->
                            { x = state.location.x, y = state.location.y - 1 }

                        '<' ->
                            { x = state.location.x - 1, y = state.location.y }

                        '>' ->
                            { x = state.location.x + 1, y = state.location.y }

                        _ ->
                            state.location

                currentPresents = 
                    Dict.get (state.location.x, state.location.y) state.grid 
                    |> Maybe.withDefault 0
            
            in
                { grid = Dict.insert (state.location.x, state.location.y) (currentPresents + 1) state.grid, location = updatedCoordinates }

        ) { grid = Dict.empty, santaLocation = { x = 0, y = 0 }, roboLocation = { x = 0, y = 0 }, which = Santa } inputVal
        |> (\state ->
                Dict.values state.grid
                |> List.length
            )

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

