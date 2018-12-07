module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, token)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Debug exposing (..)

type alias Specification =
    { x : Int
    , y : Int
    , width : Int
    , height : Int }

defaultSpecification : Specification
defaultSpecification =
    { x = 0
    , y = 0
    , width = 0
    , height = 0 }

specification : Parser Specification
specification = 
    succeed Specification
        |. symbol "#"
        |. int
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

insertOrUpdateInch : Int -> Int -> Dict String Int -> Dict String Int
insertOrUpdateInch y x tapestry =
    let
        key = String.fromInt x ++ "." ++ String.fromInt y

        exists = Dict.member key tapestry
    in
        case exists of
            True ->
                Dict.update key (Maybe.map (\val -> val + 1)) tapestry

            False ->
                Dict.insert key 1 tapestry

produceRow : Int -> Int -> Int -> Dict String Int -> Dict String Int
produceRow startX width startY tapestry =
        List.range startX (startX + (width - 1))
            |> List.foldl (insertOrUpdateInch startY) tapestry 

markArea : String -> Dict String Int -> Dict String Int
markArea string tapestry = 
    let 
        spec = case Parser.run specification string of
            Ok aSpec -> 
                aSpec
            Err err ->
                defaultSpecification

    in 
        List.range spec.y (spec.y + (spec.height - 1))
            |> List.foldl (produceRow spec.x spec.width) tapestry

createTapestry : List String -> Dict String Int -> Dict String Int
createTapestry strings tapestry =
    List.foldl markArea tapestry strings

calculate : String -> Int
calculate inputVal =
    let
        lines = String.lines inputVal

    in
        if List.isEmpty lines then
            0
        else
            createTapestry lines Dict.empty
            |> Dict.values 
            |> List.foldl (\current total -> if current > 1 then total + 1 else total) 0

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

