module Main exposing (main)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, chompIf, spaces, int, token, oneOf, map, end, getChompedString)
import Browser
import Html exposing (Html, button, div, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import List exposing (..)
import List.Extra exposing (..)
import Set exposing (..)
import Debug exposing (..)

showDebug = False

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> { message : String, time : Int }
calculate inputVal =
    let 
        lines =
            String.lines inputVal

    in
        List.foldl
            (\line vectors ->
                case Parser.run lineformat line of
                    Err err ->
                        let 
                            debug = Debug.log "error" err
                        
                        in
                            defaultVector :: vectors

                    Ok vector ->
                        vector :: vectors
            ) [] lines
        |> findMessage

myInt : Parser Int
myInt =
  oneOf
    [ succeed negate
        |. symbol "-"
        |= int
    , int
    ]

findMessage : List Vector -> { message : String, time : Int }
findMessage vectors =
    -- Seems like the message should be fairly compact. Calculate a likely starting point that slightly bigger than we expect.
    let
        firstVector =
            List.head vectors |> Maybe.withDefault defaultVector

        -- bracket the possibilities 500 on either side
        initialTime = 
            abs (((calculateExtent vectors).xExtent // 2) // 5) - 500
            |> maybeDebug "initialTime"

        state =
            {
                mostCompactVectors = []
                , xMinExtent = 10000000
                , yMinExtent = 1000000
                , xMin = 1000000
                , yMin = 1000000
                , bestTime = 0
            }

        mostCompact = 
            List.foldl
                (\time s ->
                    let
                        currentVectorsAtTime =
                            findVectorStateForTime time vectors

                        current =
                            calculateExtent currentVectorsAtTime

                    in
                        if s.xMinExtent > current.xExtent && s.yMinExtent > current.yExtent then
                            {s 
                            | mostCompactVectors = currentVectorsAtTime
                            , xMinExtent = current.xExtent
                            , yMinExtent = current.yExtent
                            , xMin = current.xMin
                            , yMin = current.yMin
                            , bestTime = time}
                        else
                            s

                ) state (List.range initialTime (initialTime+1000))
            |> maybeDebug "mostCompact"

    in
        constructMessage mostCompact

constructMessage : { a | bestTime : Int, mostCompactVectors : List Vector, xMinExtent : Int, yMinExtent : Int, xMin : Int, yMin : Int } -> { message : String, time : Int }
constructMessage state =
    let
        debuggg = 
            maybeDebug "state" state

        sortedVectors = 
            List.sortBy .y state.mostCompactVectors
            |> maybeDebug "sorted"

        firstVector =
            List.head sortedVectors |> Maybe.withDefault defaultVector

        emptyLine = 
            String.repeat state.xMinExtent "."
            |> maybeDebug "empty line"

        lines = 
            List.foldl 
                (\vector result ->
                    let
                        newResult = 
                            if vector.y /= result.currentY then
                                {result 
                                | currentY = vector.y
                                , list = List.append result.list [result.currentLine]
                                , currentLine = emptyLine
                                }
                            else
                                result

                        line = 
                            String.concat 
                            [
                                (String.slice 0 (vector.x - state.xMin) newResult.currentLine)
                                , "#"
                                , (String.dropLeft ((vector.x - state.xMin) + 1) newResult.currentLine)
                            ]
                    in
                        {newResult | currentLine = line}
                        |> maybeDebug "result"
                ) {list = [], currentY = firstVector.y, currentLine = emptyLine} sortedVectors
            |> (\result ->
                    { result | list = List.append result.list [result.currentLine]}
                )
        
        finishedMessage = 
            List.foldl
                (\line result ->
                    result ++ line ++ "\n"
                ) "" lines.list
    in
        {message = finishedMessage, time = state.bestTime}
            

calculateExtent : List Vector -> { xExtent : Int, yExtent : Int, xMin : Int, yMin : Int }
calculateExtent vectors = 
    let
        currentMinMaxState = 
            {
                xMin = 10000000
                , yMin = 10000000
                , xMax = -10000000
                , yMax = -10000000
            }

        minmax =
            List.foldl 
                (\vector result ->
                    {result 
                    | xMin = Basics.min result.xMin vector.x
                    , yMin = Basics.min result.yMin vector.y
                    , xMax = Basics.max result.xMax vector.x
                    , yMax = Basics.max result.yMax vector.y
                    }
                ) currentMinMaxState vectors
            |> maybeDebug "minmax"

    in
        { 
            xExtent = minmax.xMax - minmax.xMin
            , yExtent = minmax.yMax - minmax.yMin
            , xMin = minmax.xMin
            , yMin = minmax.yMin
        }

findVectorStateForTime : Int -> List Vector -> List Vector
findVectorStateForTime time vectors =
    List.foldl  
        (\vector updated ->
            {vector 
            | x = vector.x + (vector.dx * time)
            , y = vector.y + (vector.dy * time)
            } :: updated
        ) [] vectors

type alias Vector =
    { x : Int
    , y : Int
    , dx : Int
    , dy : Int }

defaultVector : Vector
defaultVector = 
    { x = 0
    , y = 0
    , dx = 0
    , dy = 0 }

{- position=< 1,  4> velocity=< 2,  1> -}
lineformat : Parser Vector
lineformat =
    succeed Vector
        |. token "position=<"
        |. spaces
        |= myInt
        |. token ","
        |. spaces
        |= myInt
        |. token "> velocity=<"
        |. spaces
        |= myInt
        |. token ","
        |. spaces
        |= myInt

type alias Model =
    { initialState : List Vector
    , currentState : List Vector
    , message : String
    , time : Int }

initialModel : Model
initialModel =
    { initialState = []
    , currentState = []
    , message = ". . . . . # . . . ." 
    , time = 0}

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            let
                answer = 
                    calculate newValue

                message = 
                    answer.message

                time = 
                    answer.time
            in
                { model | message = message, time = time }

view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [text <| String.fromInt model.time]
        , pre [] [ text model.message ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

