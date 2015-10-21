module View where
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import String exposing (..)
import Types exposing (Action(..), Model)
import Seq exposing (..)
import Data exposing (..)

row_ : List Html -> Html
row_ = div [ A.class "row" ]

container_ : List Html -> Html
container_ = div [ A.class "container" ]

stylesheet : String -> Html
stylesheet href =
  node "link"
  [ A.rel "stylesheet"
  , A.href href
  ] []

buttonMargin = 
  A.style [("margin-right", "5px")]

btnPrimary_ : String -> Signal.Address a -> a -> Html
btnPrimary_  label addr x =
  button [ A.class "btn btn-primary", buttonMargin, onClick addr x ]
  [ text label ]

addButton address c =
  btnPrimary_ c address (AddChar c) 

addButtons address answer =
  List.map (\c -> addButton address (String.fromChar c)) (uniqueChars answer)

picture model =
  row_ [div [A.class "col-md-4", A.style [ ( "margin-bottom","10px"), ("margin-top", "25px") ]] 
  [ img [ A.src (.image (nth model.currentIndex alternatives defaultAlternative))
  , A.width 250
  , A.height 250
  , A.style [("border","2px solid #999"), ("border-radius", "25px"), ("padding", "10px")] ] [] ] ]

controlButton adr action icon =
  button [A.class "btn btn-warning", buttonMargin, onClick adr action ] 
    [ span [A.class ("glyphicon " ++ icon)] [ ] ]

textControls address model =
  row_ [ div [A.class "col-md-4" ]
      [ controlButton address Reset "glyphicon-refresh"
      , controlButton address Backspace "glyphicon-erase"]]

guess model =
  row_ [ h1 [A.class "col-md-4"] [text (toString model.guess) ]]

letterButtons address model =
  row_ [ div [A.class "col-md-4"] (addButtons address model.answer)]

success address model =
  row_ [ div [A.class "col-md-4" ] (checkAnswer address model)]

checkAnswer : Signal.Address Action -> Model -> List Html
checkAnswer address model  =
  if model.guess == model.answer then
     [ h2 [A.style [( "color", "#49A")]] [text "Rätt svar!"]
     , button [A.class "btn btn-success", onClick address NewWord]
       [ span [A.class "glyphicon glyphicon-random"] []]
       ]
  else
   [div [] []]

view address model =
  container_
  [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , picture model
    , textControls address model
    , guess model
    , letterButtons address model
    , success address model 
    ]

