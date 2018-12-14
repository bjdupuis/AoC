module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
import Deque exposing (..)
import Set exposing (..)
import Debug exposing (..)

showDebug = False

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

        marbleDeque =
            Deque.singleton 0
        
        elves =
            Dict.empty

    in
        List.foldl 
            (\marble state ->
                let
--                    startingState = maybeDebug "starting state" state
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
                                (marbleValueThatWillBeRemoved, updatedDeque) =
                                    case Deque.popFront (rotateRight 7 state.marbleDeque) of
                                        (Nothing, d) ->
                                            (0, d)
                                        (Just val, d) ->
                                            (val, d)

                                currentElfCurrentScore =
                                    case Dict.get state.currentElf state.elves of
                                        Nothing ->
                                            0

                                        Just score ->
                                            score
                                
                                updatedElves = 
                                    Dict.insert state.currentElf (currentElfCurrentScore + marble + marbleValueThatWillBeRemoved) state.elves

                            in
                                { state 
                                | currentElf = nextElf
                                , marbleDeque = updatedDeque
                                , elves = updatedElves}
--                                |> maybeDebug ("someone scored on " ++ String.fromInt marble)
                        _ ->
                            let
                                updatedDeque = 
                                    Deque.pushFront marble (rotateLeft 2 state.marbleDeque)

                            in
                                { state 
                                | currentElf = nextElf
                                , marbleDeque = updatedDeque }
                            {- |> maybeDebug "updatedState"-}

            )  
            { game = game
            , currentElf = 1
            , elves = elves
            , marbleDeque = marbleDeque
            } marblesToPlay

    |> getElves
    |> Dict.values
    |> List.foldl
        (\elfScore maximum ->
            Basics.max elfScore maximum
        ) 0

rotateRight : Int -> Deque Int -> Deque Int
rotateRight count deque =
    List.foldl 
        (\_ d ->
            let
                (back, updated) = 
                    case Deque.popBack d of
                        (Nothing, u) ->
                            (0, u)
                        (Just b, u) ->
                            (b, u)

            in
                Deque.pushFront back updated

        ) deque (List.range 1 count)

rotateLeft : Int -> Deque Int -> Deque Int
rotateLeft count deque =
    List.foldl 
        (\_ d ->
            let
                (front, updated) = 
                    case Deque.popFront d of
                        (Nothing, u) ->
                            (0, u)
                        (Just b, u) ->
                            (b, u)

            in
                Deque.pushBack front updated

        ) deque (List.range 1 count)

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

