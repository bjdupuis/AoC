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
import Debug exposing (..)

showDebug = True

maybeDebug text a =
    if showDebug then
        Debug.log text a
    else
        a

calculate : String -> (Int, Int)
calculate inputVal =
    let
        tracksAndCarts = 
            String.lines inputVal
            |> List.foldl
                (\line outer ->
                    let
                        trackAndCarts =
                            String.foldl
                                (\char inner ->
                                    let 
                                        result = 
                                            case char of
                                                '^' ->
                                                    { inner 
                                                    | track = inner.track ++ "|"
                                                    , carts = { position = (outer.y, inner.x), currentDirection = String.fromChar char, nextIntersectionDirection = Left } :: inner.carts }

                                                'v' ->
                                                    { inner 
                                                    | track = inner.track ++ "|"
                                                    , carts = { position = (outer.y, inner.x), currentDirection = String.fromChar char, nextIntersectionDirection = Left } :: inner.carts }

                                                '<' ->
                                                    { inner 
                                                    | track = inner.track ++ "-"
                                                    , carts = { position = (outer.y, inner.x), currentDirection = String.fromChar char, nextIntersectionDirection = Left } :: inner.carts }

                                                '>' ->
                                                    { inner 
                                                    | track = inner.track ++ "-"
                                                    , carts = { position = (outer.y, inner.x), currentDirection = String.fromChar char, nextIntersectionDirection = Left } :: inner.carts }

                                                val ->
                                                    { inner
                                                    | track = inner.track ++ String.fromChar val
                                                    }
                                    in
                                        { result | x = inner.x + 1 }
                                ) { carts = [], track = "", x = 0 } line
                    in
                        { outer 
                        | tracks = outer.tracks ++ [trackAndCarts.track]
                        , carts = outer.carts ++ trackAndCarts.carts
                        , y = outer.y + 1 }

                ) { tracks = [], carts = [], y = 0 }

        debug = 
            maybeDebug "tracksAndCarts" tracksAndCarts
    in
        letTheCartsRoll { tracks = Array.fromList tracksAndCarts.tracks, carts = tracksAndCarts.carts }
        |> (\result ->
                (Tuple.second result.collisionPosition, Tuple.first result.collisionPosition)
            )

letTheCartsRoll : { a | tracks : Array String, carts : List Cart } -> { tracks : Array String, carts : List Cart, collisionPosition : (Int, Int) }
letTheCartsRoll currentState =
    let
        sortedCarts = 
            List.sortBy .position currentState.carts

        updatedState = 
            List.foldl
                (\cart result ->
                    let
                        currentTrackUnderCart =
                            Array.get (Tuple.first cart.position) currentState.tracks 
                            |> Maybe.withDefault ""
                            |> String.dropLeft (Tuple.second cart.position)
                            |> String.left 1

                        updatedCart =
                            updateCartForTimeStep cart currentTrackUnderCart currentState.tracks 

                        remainingCarts = 
                            List.tail result.carts 
                            |> Maybe.withDefault []

                        resultantCollision = 
                            if result.collisionPosition == (-1, -1) then
                                searchForCollisions updatedCart remainingCarts
                            else
                                result.collisionPosition

                        newResult =
                            remainingCarts ++ [updatedCart]
                    in
                        { result 
                        | carts = newResult
                        , collisionPosition = resultantCollision }

                ) { carts = sortedCarts, collisionPosition = (-1,-1)} sortedCarts
        log =
            maybeDebug "carts" updatedState.carts
    in
        if Tuple.first updatedState.collisionPosition >= 0 then
            { tracks = currentState.tracks
            , carts = updatedState.carts
            , collisionPosition = updatedState.collisionPosition }
        else
            letTheCartsRoll { currentState | carts = updatedState.carts }

searchForCollisions : Cart -> List Cart -> (Int, Int)
searchForCollisions cart otherCarts =
    let
        prospectiveCollisions =
            List.filter 
                (\prospectiveCollision ->
                    cart.position == prospectiveCollision.position
                ) otherCarts

    in
        case prospectiveCollisions of
            firstCollision :: rest ->
                firstCollision.position

            [] -> 
                (-1, -1)
        


updateCartForTimeStep : Cart -> String -> Array String -> Cart
updateCartForTimeStep cart currentTrackUnderCart tracks =
    case (cart.currentDirection, currentTrackUnderCart) of
        (_, "+") ->
            -- the dreaded intersection
            let
                newState = 
                    case (cart.currentDirection, cart.nextIntersectionDirection) of
                        (_, Straight) ->
                            let
                                newPosition = 
                                    case cart.currentDirection of
                                        "^" ->
                                            ((Tuple.first cart.position) - 1, Tuple.second cart.position)
                                        "v" ->
                                            ((Tuple.first cart.position) + 1, Tuple.second cart.position)
                                        "<" ->
                                            (Tuple.first cart.position, (Tuple.second cart.position) - 1)
                                        ">" ->
                                            (Tuple.first cart.position, (Tuple.second cart.position) + 1)
                                        _ ->
                                            cart.position
                                in
                                    { direction = cart.currentDirection, next = Right, position = newPosition }
                        ("^", Left) ->
                            { direction = "<", next = Straight, position = (Tuple.first cart.position, Tuple.second cart.position - 1) }
                        ("^", Right) ->
                            { direction = ">", next = Left, position = (Tuple.first cart.position, Tuple.second cart.position + 1) }
                        (">", Left) ->
                            { direction = "^", next = Straight, position = (Tuple.first cart.position - 1, Tuple.second cart.position) }
                        (">", Right) ->
                            { direction = "v", next = Left, position = (Tuple.first cart.position + 1, Tuple.second cart.position) }
                        ("v", Left) ->
                            { direction = ">", next = Straight, position = (Tuple.first cart.position, Tuple.second cart.position + 1) }
                        ("v", Right) ->
                            { direction = "<", next = Left, position = (Tuple.first cart.position, Tuple.second cart.position - 1) }
                        ("<", Left) ->
                            { direction = "v", next = Straight, position = (Tuple.first cart.position + 1, Tuple.second cart.position) }
                        ("<", Right) ->
                            { direction = "^", next = Left, position = (Tuple.first cart.position - 1, Tuple.second cart.position) }
                        (_, _) ->
                            { direction = cart.currentDirection
                            , next = cart.nextIntersectionDirection
                            , position = cart.position }
            in
                { cart 
                | currentDirection = newState.direction
                , nextIntersectionDirection = newState.next
                , position = newState.position }

        ("^", "|") ->
            -- just moving north still
            { cart 
            | position = (Tuple.first cart.position - 1, Tuple.second cart.position)
            }

        ("v", "|") ->
            -- just moving south still
            { cart 
            | position = (Tuple.first cart.position + 1, Tuple.second cart.position)
            }

        ("<", "-") ->
            -- just moving west still
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position - 1)
            }

        (">", "-") ->
            -- just moving east still
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position + 1)
            }

        ("^", "/") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position + 1)
            , currentDirection = ">"
            }

        ("^", "\\") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position - 1)
            , currentDirection = "<"
            }

        ("v", "/") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position - 1)
            , currentDirection = "<"
            }


        ("v", "\\") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position, Tuple.second cart.position + 1)
            , currentDirection = ">"
            }

        (">", "/") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position - 1, Tuple.second cart.position)
            , currentDirection = "^"
            }

        (">", "\\") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position + 1, Tuple.second cart.position)
            , currentDirection = "v"
            }

        ("<", "/") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position + 1, Tuple.second cart.position)
            , currentDirection = "v"
            }

        ("<", "\\") ->
            -- going into a turn
            { cart 
            | position = (Tuple.first cart.position - 1, Tuple.second cart.position)
            , currentDirection = "^"
            }

        (_, _) ->
            cart


type Intersection =
    Left | Straight | Right

type alias Cart =
    { position : (Int, Int)
    , currentDirection : String
    , nextIntersectionDirection : Intersection }

type alias Model =
    { result : (Int, Int) }

initialModel : Model
initialModel =
    { result = (0,0) }

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
          div [] [text <| (String.fromInt (Tuple.first model.result) ++ "," ++ String.fromInt (Tuple.second model.result)) ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

