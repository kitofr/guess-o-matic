module View where
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import String exposing (..)
import Set exposing (..)
import Types exposing (Action(..), Model)
import Seq exposing (..)
import Data exposing (..)
import Debug exposing (..)

row_ : List Html -> Html
row_ = div [ A.class "row", rowDistance ]

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

rowDistance = 
  A.style [ ( "margin-bottom","10px"), ("margin-top", "25px") ]

btnPrimary_ : String -> Signal.Address a -> a -> Html
btnPrimary_  label addr x =
  button [ A.class "btn btn-primary", buttonMargin, onClick addr x ]
  [ text label ]

addButton address c =
  btnPrimary_ c address (AddChar c) 

addButtons address answer =
  List.map (\c -> addButton address (String.fromChar c)) (uniqueChars answer)

currentImage model = 
  (.image (nth 0 model.wordList defaultAlternative))

currentAnswer model =
  (.word (nth 0 model.wordList defaultAlternative))

hasMoreWords : Model -> Bool
hasMoreWords model =
   Debug.watch "hasMoreWords" ((List.length model.wordList) > 1)

picture model =
  row_ [div [A.class "col-md-4"] 
  [ img [ A.src (currentImage model)
  , A.width 250
  , A.height 250
  , A.style [("border","7px solid #49A"), 
             ("border-radius", "25px"), 
             ("padding", "10px"),
             ("background", "#8AC")] ] [] ] ]

controlButton adr action icon =
  button [A.class "btn btn-warning", buttonMargin, onClick adr action ] 
    [ span [A.class ("glyphicon " ++ icon)] [ ] ]

textControls address model =
  row_ [ div [A.class "col-md-4" ]
      [ controlButton address Reset "glyphicon-refresh"
      , controlButton address Backspace "glyphicon-erase"]]

disabledButton ch =
  let t = String.fromChar ch
  in
     button [ A.class "btn btn-disabled", buttonMargin ] [ text t ]

paddUpTo lst n =
  if List.length lst < n then
     paddUpTo (List.append lst ['_']) n
  else
    lst

guess model =
  let answer = Debug.watch "answer" (String.toList (currentAnswer model)) 
      paddedGuess = Debug.watch "guess" (paddUpTo (String.toList model.guess) (List.length answer))
  in
  row_ [ div [A.class "col-md-4"] 
           (List.map disabledButton paddedGuess) ]

letterButtons address model =
  row_ [ div [A.class "col-md-4"] (addButtons address (currentAnswer model))]

success address model =
  row_ [ div [A.class "col-md-4" ] (checkAnswer address model)]

collectChars : Model -> Model
collectChars model =
  let collected = Debug.watch "collected" (addChars (currentAnswer model) model.collectedChars )
  in
     { model | collectedChars <- collected }

checkAnswer : Signal.Address Action -> Model -> List Html
checkAnswer address model =
  let letters = Debug.watch "collect as list" (String.join "," (List.map String.fromChar (Set.toList model.collectedChars)))
  in
  if model.guess == (currentAnswer model) then
    if (hasMoreWords model) then
     [ h2 [A.style [( "color", "#49A")]] [text "Rätt svar!"]
     , button [A.class "btn btn-success", onClick address NewWord]
       [ span [A.class "glyphicon glyphicon-thumbs-up"] []]]
     else
      [ h2 [A.style [( "color", "#4A9")]] 
        [text letters]]
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

