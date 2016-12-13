port module Ports exposing (playChar, correct)


port playChar : String -> Cmd msg


port correct : Bool -> Cmd msg
