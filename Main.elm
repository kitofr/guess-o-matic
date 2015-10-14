import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)


main =
  start { model = init "APA" ""
        , view = view
        , update = update }


type alias Model = { answer : String, guess : String}

init : String -> String -> Model
init answer guess =
  { answer = answer
  , guess = guess
  }

view address model =
  div []
    [ button [ onClick address AddA ] [ text "A" ]
    , button [ onClick address AddP ] [ text "P" ]
    , div [] [ text (toString model.guess) ]
    ]


type Action = AddA | AddP


update action model =
  case action of
    AddA -> { model | guess <- model.guess ++ "A" }
    AddP -> { model | guess <- model.guess ++ "P" }
