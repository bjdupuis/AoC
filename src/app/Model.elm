module Model exposing (Model, Msg(..))

import File exposing (File)
import Http


type Msg
    = NoOp
    | SelectedFile (List File)
    | ProcessFile String


type alias Model =
    { files : List File
    , result : String
    }
