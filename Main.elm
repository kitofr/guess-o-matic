import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)
import String exposing (..)
import Set exposing (..)
import List exposing (..)


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

uniqueChars string =
  List.foldr (\c a-> Set.insert c a) Set.empty 
  (String.toUpper string
    |> String.toList 
    |> List.sort)
  |> Set.toList

addButton address c =
    button [ onClick address (AddChar c) ] [ text c]

addButtons address answer =
  List.map (\c -> addButton address (String.fromChar c)) (uniqueChars answer)

  

view address model =
  div []
    [ div [] [ text (toString model.guess) ]
    , div [] (addButtons address model.answer)
    , div [] [ text (checkAnswer model)]]


type Action = AddChar String

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
