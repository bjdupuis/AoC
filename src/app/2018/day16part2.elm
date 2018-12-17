module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
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

        parseResult = 
            parseLines lines { possibleOpcodes = Dict.empty, testProgramLines = [] }

        opcodeFunctions =
            determineOpcodeFunctions { possibleOpcodes = parseResult.possibleOpcodes, currentFunctions = Array.repeat 16 addi }
            |> (\record ->
                    record.currentFunctions
                )
    in
        executeTestProgram opcodeFunctions (registers 0 0 0 0) parseResult.testProgramLines

executeTestProgram : Array (Int -> Int -> Int -> Registers -> Registers) -> Registers -> List String -> Int
executeTestProgram functionArray regs lines =
    List.foldl 
        (\line inner ->
            let
                instructions = 
                    case Parser.run instructionParser line of
                        Err err ->
                            let
                                debug =
                                    maybeDebug "err" err
                            in
                                Array.fromList [0,0,0,0]

                        Ok instr ->
                            instr
                            |> maybeDebug "instruction"

                opcode = 
                    getValue 0 instructions

                function = 
                    case Array.get opcode functionArray of
                        Nothing ->
                            let
                                debug = 
                                    Debug.log "error, function not found" opcode

                            in
                                addi
                        Just func ->
                            func

            in
                function 
                    (getValue 1 instructions) 
                    (getValue 2 instructions) 
                    (getValue 3 instructions) 
                    inner
                |> maybeDebug "current"

        ) regs lines
    |> getReg 0

determineOpcodeFunctions : { possibleOpcodes : Dict Int (Set Int), currentFunctions : Array (Int -> Int -> Int -> Registers -> Registers) } -> { possibleOpcodes : Dict Int (Set Int), currentFunctions : Array (Int -> Int -> Int -> Registers -> Registers)}
determineOpcodeFunctions current = 
    let
        singleFunctionOpcodes =
            Dict.filter 
                (\key value ->
                    Set.size value == 1
                ) current.possibleOpcodes
            |> maybeDebug "singleFunction"


        remainingFunctionIndices =
            Dict.foldl 
                (\_ value combined ->
                    Set.union combined value
                ) Set.empty current.possibleOpcodes

        onlyOnePossibilityOpcodes =
            Set.foldl
                (\functionIndex list ->
                    Dict.filter
                        (\_ value ->
                            Set.member functionIndex value
                        ) current.possibleOpcodes
                    |> (\dict ->
                            if Dict.size dict == 1 then
                                let
                                    opcode =
                                        Dict.keys dict
                                        |> List.head
                                        |> Maybe.withDefault 0

                                in
                                    (opcode, functionIndex) :: list
                            else
                                list
                        )

                ) [] remainingFunctionIndices
            |> maybeDebug "only one"

    in
        if Dict.size singleFunctionOpcodes > 0 then
            Dict.foldl
                (\key value result ->
                    let
                        opcode =
                            key

                        functionIndex = 
                            value
                            |> Set.toList
                            |> List.head
                            |> Maybe.withDefault 0

                        function = 
                            List.Extra.getAt functionIndex instructionRunners
                            |> Maybe.withDefault addr

                        updatedFunctions = 
                            Array.set opcode function result.currentFunctions

                        updatedPossibleOpcodes =
                            Dict.remove opcode result.possibleOpcodes
                            |> Dict.map
                                (\_ v ->
                                    Set.remove functionIndex v 
                                ) 

                    in
                        { result 
                        | possibleOpcodes = updatedPossibleOpcodes
                        , currentFunctions = updatedFunctions
                        }
                ) current singleFunctionOpcodes
            |> determineOpcodeFunctions
            |> maybeDebug "recursing single function"

        else if List.length onlyOnePossibilityOpcodes > 0 then
            List.foldl
                (\tuple result ->
                    let
                        opcode =
                            Tuple.first tuple

                        functionIndex = 
                            Tuple.second tuple

                        function = 
                            List.Extra.getAt functionIndex instructionRunners
                            |> Maybe.withDefault addr

                        updatedFunctions = 
                            Array.set opcode function result.currentFunctions

                        updatedPossibleOpcodes =
                            Dict.remove opcode result.possibleOpcodes
                            |> Dict.map
                                (\key value ->
                                    Set.remove functionIndex value 
                                ) 
                    in
                        { result 
                        | possibleOpcodes = updatedPossibleOpcodes
                        , currentFunctions = updatedFunctions
                        }
                ) current onlyOnePossibilityOpcodes
            |> determineOpcodeFunctions
            |> maybeDebug "recursing one possibility"

        else
            current
            |> maybeDebug "final opcode determination"

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


parseLines : List String -> { possibleOpcodes : Dict Int (Set Int), testProgramLines : List String } -> { possibleOpcodes : Dict Int (Set Int), testProgramLines : List String }
parseLines strings current =
    case strings of
        before :: instruction :: after :: blank :: rest ->
            if String.startsWith "Before" before then
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

                    testResults =
                        List.indexedMap
                            (\index function -> 
                                (index, 
                                function 
                                    (getValue 1 instructions) 
                                    (getValue 2 instructions) 
                                    (getValue 3 instructions) 
                                    registersBefore)
                            ) instructionRunners
                        |> List.filter
                            (\value ->
                                Tuple.second value == registersAfter
                            )
                        |> List.foldl
                            (\result inner ->
                                let
                                    opcode = 
                                        getValue 0 instructions

                                    existingOpcodes = 
                                        case Dict.get opcode inner.possibleOpcodes of
                                            Nothing ->
                                                Set.empty

                                            Just s ->
                                                s
                                in
                                    { inner 
                                    | possibleOpcodes = Dict.insert opcode (Set.insert (Tuple.first result) existingOpcodes) inner.possibleOpcodes
                                    }
                            ) current 
                    
                in
                    parseLines rest testResults
--                    |> maybeDebug "results"
            else
                { current 
                | testProgramLines = [after] ++ [blank] ++ rest }
                    |> maybeDebug "opcodes"

        _ ->
            current
    

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
            let
                debug = Debug.log "error getting register number " registerNumber
            in
                0

        Just val ->
            val

getValue : Int -> Instruction -> Int
getValue instructionPart instruction =
    case Array.get instructionPart instruction of
        Nothing ->
            let
                debug = Debug.log "error getting instruction part " instructionPart
            in
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
    |> maybeDebug "addi"

mulr : Int -> Int -> Int -> Registers -> Registers
mulr a b c regs =
    Array.set c (getReg a regs * getReg b regs) regs
    |> maybeDebug "mulr"

muli : Int -> Int -> Int -> Registers -> Registers
muli a b c regs =
    Array.set c (getReg a regs * b) regs
    |> maybeDebug "muli"

banr : Int -> Int -> Int -> Registers -> Registers
banr a b c regs =
    Array.set c (Bitwise.and (getReg a regs) (getReg b regs)) regs
    |> maybeDebug "banr"

bani : Int -> Int -> Int -> Registers -> Registers
bani a b c regs =
    Array.set c (Bitwise.and (getReg a regs) b) regs
    |> maybeDebug "bani"

borr : Int -> Int -> Int -> Registers -> Registers
borr a b c regs =
    Array.set c (Bitwise.or (getReg a regs) (getReg b regs)) regs
    |> maybeDebug "borr"

bori : Int -> Int -> Int -> Registers -> Registers
bori a b c regs =
    Array.set c (Bitwise.or (getReg a regs) b) regs
    |> maybeDebug "bori"

setr : Int -> Int -> Int -> Registers -> Registers
setr a _ c regs =
    Array.set c (getReg a regs) regs
    |> maybeDebug "setr"

seti : Int -> Int -> Int -> Registers -> Registers
seti a _ c regs =
    Array.set c a regs
    |> maybeDebug "seti"

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
        |> maybeDebug "gtir"

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
        |> maybeDebug "gtri"


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
        |> maybeDebug "gtrr"

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
        |> maybeDebug "eqir"

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
        |> maybeDebug "eqri"


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
        |> maybeDebug "eqrr"

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

