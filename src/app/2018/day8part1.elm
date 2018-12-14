module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import Debug exposing (..)

calculate : String -> Int
calculate inputVal =
    let
        answer =  parseNode {list = (String.split " " inputVal), total = 0}
        
    in
        answer.total

type alias Data =
    {list : List String
    , total : Int}

defaultData : Data 
defaultData = 
    {list = []
    , total = 0}

parseNode : Data -> Data
parseNode data =
    case data.list of
        head :: next :: rest ->
            let
                numberOfChildrenNodes = String.toInt head |> Maybe.withDefault 0

                log1 = Debug.log "numberOfChildrenNodes" numberOfChildrenNodes

                numberOfMetadata = String.toInt next |> Maybe.withDefault 0

                log2 = Debug.log "numberOfMetadata" numberOfMetadata

                childRange = List.range 1 numberOfChildrenNodes

                metadataRange = List.range 1 numberOfMetadata

                childrenResults = 
                    case numberOfChildrenNodes of
                        0 ->
                            {data | list = rest}
                        _ ->
                            List.foldl
                                (\value result ->
                                    let
                                        log = Debug.log "result" result
                                    in
                                        parseNode result
                                ) {data | list = rest}  childRange

            in
                List.foldl
                    (\value result ->
                        case result.list of
                            metadata :: remainder ->
                                {result | total = result.total + (String.toInt metadata |> Maybe.withDefault 0), list = remainder}
                                |> Debug.log "metaResult"
                            [] ->
                                result
                    ) childrenResults metadataRange

        _ ->
            data 

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

