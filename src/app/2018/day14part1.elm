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

showDebug = False

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> String
calculate inputVal =
    let
        numberOfRecipes =
            String.toInt inputVal
            |> Maybe.withDefault 0

        elfCurrentRecipePositions = 
            Array.fromList [0, 1]

        initialRecipes = 
            Dict.fromList [
                (0, 3),
                (1, 7)
            ]

    in
        createRecipes (numberOfRecipes + 10) elfCurrentRecipePositions initialRecipes

createRecipes : Int -> Array Int -> Dict Int Int -> String
createRecipes numberOfRecipesToCreate currentRecipePositions recipes =
    if Dict.size recipes >= numberOfRecipesToCreate then
        List.foldl
            (\recipeIndex result ->
                let 
                    recipe = 
                        case Dict.get recipeIndex recipes of
                            Nothing ->
                                0
                            Just r ->
                                r
                in
                    result ++ String.fromInt (recipe)
            ) "" (List.range (numberOfRecipesToCreate - 10) (numberOfRecipesToCreate - 1))
    else
        let
            newRecipes = 
                Array.foldl
                    (\recipePosition total ->
                        let
                            recipe = Dict.get recipePosition recipes
                                |> Maybe.withDefault 0

                        in
                            total + recipe

                    ) 0 currentRecipePositions
                |> (\recipe ->
                        if recipe >= 10 then
                            [recipe // 10, remainderBy 10 recipe ]
                        else
                            [recipe]
                    )
                |> maybeDebug "newRecipes"
            
            totalRecipes =
                List.foldl
                    (\recipe result ->
                        Dict.insert (Dict.size result) recipe result
                    ) recipes newRecipes
                |> maybeDebug "totalRecipes"

            logit =
                if modBy 1000 (Dict.size totalRecipes) == 0 then
                    Debug.log "total recipes" (Dict.size totalRecipes)
                else
                    0

            newRecipePositions = 
                Array.foldl
                    (\oldRecipePosition newPositions ->
                        let
                            oldRecipe = 
                                case Dict.get oldRecipePosition totalRecipes of
                                    Nothing ->
                                        0
                                    Just xs ->
                                        xs

                            moveBy = 
                                oldRecipePosition + oldRecipe + 1
                        in
                            if Dict.size totalRecipes > moveBy then
                                Array.push moveBy newPositions
                            else
                                Array.push (remainderBy (Dict.size totalRecipes) moveBy) newPositions
                        
                    ) Array.empty currentRecipePositions
                |> maybeDebug "newRecipePositions"

        in
            createRecipes numberOfRecipesToCreate newRecipePositions totalRecipes
        

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

