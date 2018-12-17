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
import Bitwise exposing (..)
import Debug exposing (..)

showDebug = True

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> Int
calculate inputVal =
    let
        lines = 
            String.lines inputVal

    in
        parseLines lines 0

instructionRunners : List (Int -> Int -> Int -> Registers -> Registers)
instructionRunners =
    [ addi
    , addr
    , muli
    , mulr
    , bani
    , banr
    , bori
    , borr
    , seti
    , setr
    , gtir
    , gtri
    , gtrr
    , eqri
    , eqir
    , eqrr ]

parseLines : List String -> Int -> Int
parseLines strings total =
    case strings of
        before :: instruction :: after :: blank :: rest ->
            let
                registersBefore = 
                    case Parser.run beforeParser before of
                        Err err ->
                            let
                                debug =
                                    maybeDebug "err" err
                            in
                                Array.fromList [0,0,0,0]

                        Ok regs ->
                            regs
                            |> maybeDebug "regsBefore"

                registersAfter = 
                    case Parser.run afterParser after of
                        Err err ->
                            let
                                debug =
                                    maybeDebug "err" err
                            in
                                Array.fromList [0,0,0,0]

                        Ok regs ->
                            regs
                            |> maybeDebug "regsAfter"

                instructions = 
                    case Parser.run instructionParser instruction of
                        Err err ->
                            let
                                debug =
                                    maybeDebug "err" err
                            in
                                Array.fromList [0,0,0,0]

                        Ok regs ->
                            regs
                            |> maybeDebug "instructions"

                isAtLeast3 =
                    List.map
                        (\function -> 
                            function 
                                (getValue 1 instructions) 
                                (getValue 2 instructions) 
                                (getValue 3 instructions) 
                                registersBefore
                        ) instructionRunners
                    |> List.filter
                        (\value ->
                            value == registersAfter
                        )
                    |> List.length
                    |> (\value ->
                            value >= 3
                        )
            in
                if isAtLeast3 then
                    parseLines rest (total + 1)
                else
                    parseLines rest total

        _ ->
            total
    

type alias Instruction =
    Array Int

type alias Registers =
    Array Int 

registers : Int -> Int -> Int -> Int -> Registers
registers r1 r2 r3  r4 =
    Array.fromList [r1, r2, r3, r4]

beforeParser : Parser Registers
beforeParser =
    succeed registers
        |. token "Before: ["
        |= int
        |. token ", "
        |= int
        |. token ", "
        |= int
        |. token ", "
        |= int

afterParser : Parser Registers
afterParser =
    succeed registers
        |. token "After:  ["
        |= int
        |. token ", "
        |= int
        |. token ", "
        |= int
        |. token ", "
        |= int

instructionParser : Parser Registers
instructionParser =
    succeed registers
        |= int
        |. token " "
        |= int
        |. token " "
        |= int
        |. token " "
        |= int

getReg : Int -> Registers -> Int
getReg registerNumber regs =
    case Array.get registerNumber regs of
        Nothing ->
            0

        Just val ->
            val

getValue : Int -> Instruction -> Int
getValue instructionPart instruction =
    case Array.get instructionPart instruction of
        Nothing ->
            0

        Just val ->
            val

addr : Int -> Int -> Int -> Registers -> Registers
addr a b c regs =
    Array.set c (getReg a regs + getReg b regs) regs
    |> maybeDebug "addr"

addi : Int -> Int -> Int -> Registers -> Registers
addi a b c regs =
    Array.set c (getReg a regs + b) regs

mulr : Int -> Int -> Int -> Registers -> Registers
mulr a b c regs =
    Array.set c (getReg a regs * getReg b regs) regs

muli : Int -> Int -> Int -> Registers -> Registers
muli a b c regs =
    Array.set c (getReg a regs * b) regs

banr : Int -> Int -> Int -> Registers -> Registers
banr a b c regs =
    Array.set c (and (getReg a regs) (getReg b regs)) regs

bani : Int -> Int -> Int -> Registers -> Registers
bani a b c regs =
    Array.set c (and (getReg a regs) b) regs

borr : Int -> Int -> Int -> Registers -> Registers
borr a b c regs =
    Array.set c (or (getReg a regs) (getReg b regs)) regs

bori : Int -> Int -> Int -> Registers -> Registers
bori a b c regs =
    Array.set c (or (getReg a regs) b) regs

setr : Int -> Int -> Int -> Registers -> Registers
setr a b c regs =
    Array.set c (getReg a regs) regs

seti : Int -> Int -> Int -> Registers -> Registers
seti a b c regs =
    Array.set c a regs

gtir : Int -> Int -> Int -> Registers -> Registers
gtir a b c regs =
    let
        compare = 
            if a > (getReg b regs) then
                1
            else
                0
    in
        Array.set c compare regs

gtri : Int -> Int -> Int -> Registers -> Registers
gtri a b c regs =
    let
        compare = 
            if (getReg a regs) > b then
                1
            else
                0
    in
        Array.set c compare regs

gtrr : Int -> Int -> Int -> Registers -> Registers
gtrr a b c regs =
    let
        compare = 
            if (getReg a regs) > (getReg b regs) then
                1
            else
                0
    in
        Array.set c compare regs

eqir : Int -> Int -> Int -> Registers -> Registers
eqir a b c regs =
    let
        compare = 
            if a == (getReg b regs) then
                1
            else
                0
    in
        Array.set c compare regs

eqri : Int -> Int -> Int -> Registers -> Registers
eqri a b c regs =
    let
        compare = 
            if (getReg a regs) == b then
                1
            else
                0
    in
        Array.set c compare regs

eqrr : Int -> Int -> Int -> Registers -> Registers
eqrr a b c regs =
    let
        compare = 
            if (getReg a regs) == (getReg b regs) then
                1
            else
                0
    in
        Array.set c compare regs

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

