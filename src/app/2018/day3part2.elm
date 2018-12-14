module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, token)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import Debug exposing (..)
import List.Extra exposing (..)

type alias Specification =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int }

defaultSpecification : Specification
defaultSpecification =
    { id = 0
    , x = 0
    , y = 0
    , width = 0
    , height = 0 }

specification : Parser Specification
specification = 
    succeed Specification
        |. symbol "#"
        |= int
        |. spaces
        |. symbol "@"
        |. spaces
        |= int 
        |. symbol ","
        |= int
        |. symbol ":"
        |. spaces
        |= int
        |. token "x"
        |= int

insertOrUpdateInch : Int -> Int -> Int -> Dict String (List Int) -> Dict String (List Int)
insertOrUpdateInch id y x tapestry =
    let
        key = String.fromInt x ++ "." ++ String.fromInt y

    in
        Dict.update key (\val -> 
            case val of 
                Nothing -> Just (List.singleton id)
                Just xs -> Just (id :: xs)
        ) tapestry

produceRow : Int -> Int -> Int -> Int -> Dict String (List Int) -> Dict String (List Int)
produceRow id startX width startY tapestry =
        List.range startX (startX + (width - 1))
            |> List.foldl (insertOrUpdateInch id startY) tapestry 

markArea : String -> Dict String (List Int) -> Dict String (List Int)
markArea string tapestry = 
    let 
        spec = case Parser.run specification string of
            Ok aSpec -> 
                aSpec
            Err err ->
                defaultSpecification

    in 
        List.range spec.y (spec.y + (spec.height - 1))
            |> List.foldl (produceRow spec.id spec.x spec.width) tapestry

createTapestry : List String -> Dict String (List Int) -> Dict String (List Int)
createTapestry strings tapestry =
        List.foldl markArea tapestry strings

isNotMember : List Int -> Int -> Bool
isNotMember list candidate =
    List.member candidate list
        |> not

removeInchesWithMultiple : Dict String (List Int) -> List Int
removeInchesWithMultiple tapestry =
    let 
        values = Dict.values tapestry

        multiples = List.filter (\inchMembers -> List.length inchMembers > 1) values
            |> List.concat
            |> List.Extra.unique

        multiplesRemoved = List.filter (\inchMembers -> List.length inchMembers == 1) values
            |> List.concat
            |> List.Extra.unique

    in
        List.filter (isNotMember multiples) multiplesRemoved

calculate : String -> Int
calculate inputVal =
    let
        lines = String.lines inputVal

    in
        if List.isEmpty lines then
            0
        else
            createTapestry lines Dict.empty
                |> removeInchesWithMultiple 
                |> List.head 
                |> Maybe.withDefault 0

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

