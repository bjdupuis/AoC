module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
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
    case Parser.run lineformat inputVal of
        Err err ->
            let 
                debug = Debug.log "error" err
            
            in
                0

        Ok gameDescription ->
            playGame gameDescription

playGame : GameDescription -> Int
playGame game =
    let
        marblesToPlay = 
            List.range 1 game.lastMarble

        marbleCircle =
            [0]
        
        elves =
            Dict.empty

    in
        List.foldl 
            (\marble state ->
                let
                    {-startingState = maybeDebug "starting state" state-}
                    currentMarbleDebug = 
                        case (modBy 1000 marble) of
                            0 ->
                                maybeDebug "current marble" marble
                            _ ->
                                marble
                    nextElf =
                        if state.currentElf == state.game.elfCount then
                            1
                        else
                            state.currentElf + 1
                in
                    case (modBy 23 marble) of
                        0 ->
                            let
                                updatedMarbleIndex = 
                                    if state.currentMarbleIndex < 7 then
                                        (List.length state.marbleCircle) - (7 - state.currentMarbleIndex)
                                    else 
                                        state.currentMarbleIndex - 7

                                marbleValueThatWillBeRemoved =
                                    case List.Extra.getAt updatedMarbleIndex state.marbleCircle of
                                        Nothing ->
                                            0
                                            |> maybeDebug ("NOTHING FOUND at " ++ String.fromInt updatedMarbleIndex)
                                        Just marbleValue ->
                                            marbleValue

                                currentElfCurrentScore =
                                    case Dict.get state.currentElf state.elves of
                                        Nothing ->
                                            0

                                        Just score ->
                                            score
                                
                                updatedElves = 
                                    Dict.insert state.currentElf (currentElfCurrentScore + marble + marbleValueThatWillBeRemoved) state.elves

                                updatedCircle =
                                    List.Extra.removeAt updatedMarbleIndex state.marbleCircle

                            in
                                { state 
                                | currentElf = nextElf
                                , marbleCircle = updatedCircle
                                , elves = updatedElves
                                , currentMarbleIndex = updatedMarbleIndex}
--                                |> maybeDebug ("someone scored on " ++ String.fromInt marble)
                        _ ->
                            let
                                updatedCircle =
                                    case (List.length state.marbleCircle) - state.currentMarbleIndex of
                                        2 ->
                                            List.append state.marbleCircle [marble]
--                                            |> maybeDebug "2 case"
                                        1 ->
                                            List.concat 
                                            [
                                                List.take 1 state.marbleCircle
                                                , [marble]
                                                , List.tail state.marbleCircle |> Maybe.withDefault []
                                            ]
                                            --|> maybeDebug "0 case"
                                        _ ->
                                            List.concat
                                            [
                                                List.take (state.currentMarbleIndex + 2) state.marbleCircle
                                                , [marble]
                                                , List.drop (state.currentMarbleIndex + 2) state.marbleCircle
                                            ]
--                                            |> maybeDebug "other case"

                                updatedMarbleIndex =
                                    case List.Extra.elemIndex marble updatedCircle of
                                        Nothing ->
                                            0 
                                            |> Debug.log "ANOTHER IMPOSSIBLE, JUST INSERTED"
                                        Just index ->
                                            index

                            in
                            { state 
                            | currentElf = nextElf
                            , currentMarbleIndex = updatedMarbleIndex
                            , marbleCircle = updatedCircle }
                            {- |> maybeDebug "updatedState"-}

            )  
            { game = game
            , currentElf = 1
            , elves = elves
            , marbleCircle = marbleCircle
            , currentMarbleIndex = 0
            } marblesToPlay

    |> getElves
    |> Dict.values
    |> List.foldl
        (\elfScore maximum ->
            Basics.max elfScore maximum
        ) 0

getElves : {a | elves : Dict Int Int} -> Dict Int Int
getElves {elves} =
    elves

{- 416 players; last marble is worth 71975 points -}
type alias GameDescription =
    { elfCount : Int
    , lastMarble : Int }

lineformat : Parser GameDescription
lineformat =
    succeed GameDescription
        |= int
        |. token " players; last marble is worth "
        |= int

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

