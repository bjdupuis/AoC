module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Set exposing (Set)

type alias Exists =
    { only2 : Bool
    , only3 : Bool }
    
defaultExists : Exists
defaultExists =
    { only2 = False
    , only3 = False }

type alias Counts =
    { only2 : Int
    , only3 : Int }

defaultCounts : Counts
defaultCounts =
    { only2 = 0
    , only3 = 0 }

determineResult : Counts -> Exists -> Counts
determineResult current exists =
    case exists.only2 of
        True ->
            case exists.only3 of
                True -> 
                    { current | only2 = current.only2 + 1, only3 = current.only3 + 1 }
                False ->
                    { current | only2 = current.only2 + 1 }
        False -> 
            case exists.only3 of
                True -> 
                    { current | only3 = current.only3 + 1 }
                False ->
                    current

recurseLine : String -> Counts -> Exists -> Counts
recurseLine inputVal current exists =
    let 
        leftmost = String.left 1 inputVal 
        
        restOfString = String.replace (String.left 1 inputVal) "" inputVal
    in 
        if String.isEmpty leftmost then
            determineResult current exists
        else if List.length (String.indices leftmost inputVal) == 3 then
            recurseLine restOfString current {exists | only3 = True } 
        else if List.length (String.indices leftmost inputVal) == 2 then
            recurseLine restOfString current {exists | only2 = True } 
        else
            recurseLine restOfString current exists 


calculateLine : String -> Counts -> Counts
calculateLine inputVal current =
    recurseLine inputVal current defaultExists

calculate : String -> Int
calculate inputValue =
    String.lines inputValue
    |> List.foldl calculateLine defaultCounts 
    |> (\current -> current.only2 * current.only3)


type alias Model =
    { checksum : Int }


initialModel : Model
initialModel =
    { checksum = 0 }

type Msg
    = Calculate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate newValue ->
            { model | checksum = calculate newValue }

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| String.fromInt model.checksum ]
        , textarea [ onInput Calculate ] []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

