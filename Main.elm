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

checkAnswer : Model -> String
checkAnswer model =
  if model.guess == model.answer then
     "Rätt!"
  else
    ""

view address model =
  div []
    [ div [] [ text (toString model.guess) ]
    , (addButtons model.answer)
    , button [ onClick address (AddChar "A") ] [ text "A" ]
    , button [ onClick address (AddChar "P") ] [ text "P" ]
    , div [] [ text (checkAnswer model)]]


type Action = AddChar String

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
