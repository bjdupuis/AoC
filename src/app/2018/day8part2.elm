module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
import Set exposing (..)
import Debug exposing (..)

calculate : String -> Int
calculate inputVal =
    let
        answer =  parseNode {defaultData | list = (String.split " " inputVal)}
        
    in
        answer.total

type alias Data =
    { list : List String
    , total : Int
    , children : List Int}

showDebug = False

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

defaultData : Data 
defaultData = 
    { list = []
    , total = 0
    , children = []}

parseNode : Data -> Data
parseNode data =
    case data.list of
        head :: next :: rest ->
            let
                numberOfChildrenNodes = String.toInt head |> Maybe.withDefault 0

                d1 = maybeDebug "numberOfChildrenNodes" numberOfChildrenNodes

                numberOfMetadata = String.toInt next |> Maybe.withDefault 0

                d2 = maybeDebug "numberOfMetadata" numberOfMetadata

                childRange = List.range 1 numberOfChildrenNodes

                metadataRange = List.range 1 numberOfMetadata

                childrenResults = 
                    case numberOfChildrenNodes of
                        0 ->
                            {data | list = rest, children = []}
                                |> maybeDebug "0 children traverse" 

                        _ ->
                            List.foldl
                                (\_ result ->
                                    let 
                                        childResult = parseNode result

                                    in
                                        {childResult | children = (List.append result.children [childResult.total])}

                                ) {data | list = rest, children = []} childRange

            in
                List.foldl
                    (\_ result ->
                        case result.list of
                            metadata :: remainder ->
                                case numberOfChildrenNodes of
                                    0 ->
                                        {result | total = result.total + (String.toInt metadata |> Maybe.withDefault 0), list = remainder}
                                            |> maybeDebug "0 children" 

                                    _ ->
                                        let
                                            meta = maybeDebug "metadata" metadata
                                            children = maybeDebug "children" result.children
                                            index = case String.toInt metadata of
                                                Nothing ->
                                                    -1
                                                Just xs ->
                                                    xs - 1
                                            childAmount = 
                                                case List.Extra.getAt index result.children of 
                                                    Just xs ->
                                                        xs
                                                    Nothing ->
                                                        0
                                            d3 = maybeDebug "childAmount" childAmount

                                        in
                                            {result | total = result.total + childAmount, list = remainder}
                                                |> maybeDebug "children" 


                            [] ->
                                result
                                    |> maybeDebug "end result" 

                    ) {childrenResults | total = 0} metadataRange
                        |> maybeDebug "folded final" 


        _ ->
            data 
                |> maybeDebug "end" 

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

