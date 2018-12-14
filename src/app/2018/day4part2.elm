module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int, spaces, token, oneOf, map, end)
import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import Set exposing (..)
import Debug exposing (..)

type Action = Starts Int | Wakes | Sleeps

type alias GuardSleepData =
    { id : Int
    , totalMinutesAsleep : Int
    , sleepByMinute : Dict Int Int}

type alias SleepiestMinute =
    { id : Int
    , minute : Int
    , sleepDuringMinute : Int }

defaultGuardSleepData : GuardSleepData 
defaultGuardSleepData = 
    { id = 0
    , totalMinutesAsleep = 0
    , sleepByMinute = Dict.empty
    }

type alias GuardData =
    { month : Int
    , day : Int
    , hours : Int
    , minutes : Int
    , action : Action }

defaultGuardData : GuardData
defaultGuardData = 
    { month = 0
    , day = 0
    , hours = 0
    , minutes = 0
    , action = Starts 0}

leadingZeroInt : Parser Int
leadingZeroInt =
    oneOf
        [ succeed identity
            |. symbol "0"
            |= int
        , int
        ]

lineformat : Parser GuardData
lineformat =
    succeed GuardData
        |. token "[1518-"
        |= leadingZeroInt 
        |. token "-"
        |= leadingZeroInt
        |. spaces
        |= leadingZeroInt
        |. token ":"
        |= leadingZeroInt
        |. token "] "
        |= oneOf
        [ succeed Wakes
            |. token "wakes up"
        , succeed Sleeps
            |. token "falls asleep"
        , succeed Starts
            |. token "Guard #"
            |= int
            |. token " begins shift"
        ]

lastDayOfMonth : Int -> Int
lastDayOfMonth month =
    case month of
        1 -> 31
        2 -> 28
        3 -> 31
        4 -> 30
        5 -> 31
        6 -> 30
        7 -> 31
        8 -> 31
        9 -> 30
        10 -> 31
        11 -> 30
        12 -> 31
        _ -> 0



adjustDay : Int -> Int -> Int
adjustDay day month = 
    if lastDayOfMonth month == day then
        1
    else
        day + 1

adjustMonth : Int -> Int -> Int
adjustMonth day month = 
    if lastDayOfMonth month == day then
        month + 1
    else
        month

parseLine : String -> GuardData
parseLine string =
    let 
        data = case Parser.run lineformat string of
            Ok aData -> 
                aData
            Err err ->
                let
                    log1 = Debug.log "err" err
                in
                    defaultGuardData

    in
        if data.hours == 23 then
            { data | hours = 0, minutes = 0, day = (adjustDay data.day data.month), month = (adjustMonth data.day data.month)}
        else
            data

updateSleepByMinute : Dict Int Int -> List Int -> Dict Int Int
updateSleepByMinute sleepByMinute range =
    List.foldl 
        (\value d1 ->
            let
                minute = 
                    case Dict.get value d1 of
                        Nothing ->
                            0
                        Just xs ->
                            xs

            in
                Dict.insert value (minute + 1) d1
        ) sleepByMinute range

processGuardData : List GuardData -> Dict Int GuardSleepData -> Maybe GuardSleepData -> Dict Int GuardSleepData
processGuardData guardDataList collectedSleepData currentSleepData  =
    let 
        current = List.head guardDataList 

        remaining = List.tail guardDataList |> Maybe.withDefault []
    in
        case current of
            Nothing ->
                collectedSleepData
            
            Just currentValue ->
                case currentValue.action of
                    Starts guardId ->
                        let
                            guardSleepData = 
                                case Dict.get guardId collectedSleepData of
                                    Nothing -> 
                                        { id = guardId, totalMinutesAsleep = 0, sleepByMinute = Dict.empty }

                                    Just xs ->
                                        xs

                        in
                            processGuardData remaining (Dict.insert guardId guardSleepData collectedSleepData) (Just guardSleepData)

                    Sleeps -> 
                        let
                            wakes = 
                                case List.head remaining of 
                                    Nothing ->
                                        defaultGuardData
                                    
                                    Just xs -> 
                                        xs


                            timeAsleep = wakes.minutes - currentValue.minutes

                            minuteRange = List.range currentValue.minutes wakes.minutes

                            afterWakeUp = List.tail guardDataList |> Maybe.withDefault []

                            sleep =
                                case currentSleepData of 
                                    Just data -> 
                                        { data | 
                                            totalMinutesAsleep = data.totalMinutesAsleep + timeAsleep, 
                                            sleepByMinute = updateSleepByMinute data.sleepByMinute minuteRange}
                                    Nothing ->
                                        { defaultGuardSleepData | totalMinutesAsleep = timeAsleep}
                        in
                            processGuardData afterWakeUp (Dict.insert sleep.id sleep collectedSleepData) (Just sleep)

                    Wakes -> 
                        let
                            sleep = 
                                case currentSleepData of 
                                    Just data -> 
                                        data
                                    Nothing ->
                                        defaultGuardSleepData
                        in
                            processGuardData remaining (Dict.insert sleep.id sleep collectedSleepData) currentSleepData


processGuardDataList : List GuardData -> Dict Int GuardSleepData
processGuardDataList list =
    processGuardData list Dict.empty Nothing 

findSleepiestGuard : List GuardSleepData -> SleepiestMinute
findSleepiestGuard guards =
    List.foldl 
                (\value max ->
                    let
                        sleepiestMinute = 
                            Dict.foldl
                                (\minute sleepDuringMinute mostDuringMinute ->
                                    if sleepDuringMinute > Tuple.second mostDuringMinute then 
                                        (minute, sleepDuringMinute)
                                    else 
                                        mostDuringMinute
                                ) (0,0) value.sleepByMinute

                        log = Debug.log "sleepiestMinute" sleepiestMinute 
                    in
                        if Tuple.second sleepiestMinute > max.sleepDuringMinute then
                            {id = value.id, minute = Tuple.first sleepiestMinute, sleepDuringMinute = Tuple.second sleepiestMinute}
                        else
                            max
                ) { id = 0, minute = 0, sleepDuringMinute = 0} guards

calculateAnswer : SleepiestMinute -> Int
calculateAnswer minute =
        minute.id * minute.minute

parseLines : List String -> Int
parseLines lines =
    let
        sorted = List.sort lines

    in
        List.map parseLine sorted
            |> Debug.log "list"
            |> processGuardDataList
            |> Debug.log "guardData"
            |> Dict.values
            |> findSleepiestGuard 
            |> Debug.log "sleepy" 
            |> calculateAnswer

calculate : String -> Int
calculate inputVal =
    let
        lines = String.lines inputVal

    in
        if List.isEmpty lines then
            0
        else
            parseLines lines

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

