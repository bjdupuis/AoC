module Main exposing (init, main, update, view)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Model exposing (..)
import Task


day1 : String -> Int
day1 data =
    let
        numbers =
            List.filterMap String.toInt (String.split "\n" data)
    in
    List.foldl (+) 0 numbers


init : ( Model, Cmd Msg )
init =
    ( { files = [], result = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { files = [], result = "" }, Cmd.none )

        SelectedFile files ->
            let
                file =
                    List.head files
            in
            case file of
                Just f ->
                    ( { files = files, result = "Processing..." }, Task.perform ProcessFile (File.toString f) )

                Nothing ->
                    ( { files = files, result = "Failure..." }, Cmd.none )

        ProcessFile data ->
            ( { model | result = Debug.toString (day1 data) }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (Decode.map SelectedFile filesDecoder)
            ]
            []
        , div [] [ text (Debug.toString model) ]
        ]



---- PROGRAM ----


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
