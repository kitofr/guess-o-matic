module View where
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import String exposing (..)
import Set exposing (..)
import Types exposing (..)
import Seq exposing (..)
import Data exposing (..)
import Debug exposing (..)

row_ : List Html -> Html
row_ = div [ A.class "row", rowDistance ]

container_ : List Html -> Html
container_ = div [ A.class "container-fluid" ]

stylesheet : String -> Html
stylesheet href =
  node "link"
  [ A.rel "stylesheet"
  , A.href href
  ] []

buttonStyle = A.style [
  ("margin-right", "10px")
  ,("width", "80px")
  ,("font-size", "20px") 
  ,("padding", "10px")]

rowDistance = 
  A.style [ ( "margin-bottom","10px"), ("margin-top", "25px") ]

btnPrimary_ : String -> Signal.Address a -> a -> Html
btnPrimary_  label addr x =
  button [ A.class "btn btn-primary"
          , buttonStyle
          , onClick addr x ]
  [ text label ]

addButton address c =
  btnPrimary_ c address (AddChar c) 

addButtons address answer =
  List.map (\c -> addButton address (String.fromChar c)) (uniqueChars answer)

image : Guess -> String
image (_,question) = 
  question.image

currentAnswer : Model -> String
currentAnswer {guess, seed, state} =
  let (_,question) = guess
  in
      word question

hasMoreWords : Model -> Bool
hasMoreWords {guess, seed, state} =
  let lst = wordList state
  in
      case lst of
        [] -> False
        otherwise -> True

picture : Model -> Html
picture {guess, seed, state} =
  row_ [div [A.class "col-md-6"] 
  [ img [ A.src (image guess)
  , A.width 500
  , A.height 500
  , A.style [("border","1px solid #AAA"), 
             ("border-radius", "25px"), 
             ("padding", "10px"),
             ("-webkit-box-shadow", "0 10px 6px -6px #777"),
             ("-moz-box-shadow", "0 10px 6px -6px #777"),
             ("box-shadow", "0 10px 6px -6px #777")
             ] ] [] ] ]
 
controlButton adr action icon =
  button [A.class "btn btn-warning", buttonStyle, onClick adr action ] 
    [ span [A.class ("glyphicon " ++ icon)
           ,buttonStyle] [ ] ]
 
textControls address model =
  row_ [ div [A.class "col-md-4" ]
      [ controlButton address Reset "glyphicon-refresh"
      , controlButton address Backspace "glyphicon-erase"]]
 
disabledButton ch =
  let t = String.fromChar ch
  in
     button [ A.class "btn btn-disabled", buttonStyle  ] [ text t ]

paddUpTo lst n =
  if List.length lst < n then
     paddUpTo (List.append lst ['_']) n
  else
    lst

answer : Guess -> String
answer (_, question) = (word question)

showGuess : Model -> Html
showGuess {guess, seed, state} =
  let answer' = Debug.watch "answer" (String.toList (answer guess)) 
      paddTo = (List.length answer')
      paddedGuess = Debug.watch "guess" (paddUpTo (String.toList (fst guess)) paddTo )
  in
  row_ [ div [A.class "col-md-4"] 
           (List.map disabledButton paddedGuess) ]

letterButtons : Signal.Address Action -> Model -> Html
letterButtons address model =
  row_ [ div [A.class "col-md-4"] (addButtons address (currentAnswer model))]

success : Signal.Address Action -> Model -> Html
success address model =
  row_ [ div [A.class "col-md-4" ] (checkAnswer address model)]

collectedCharsAsCommaSeparatedString : Set Char -> String
collectedCharsAsCommaSeparatedString collected = 
  (String.join "," (List.map String.fromChar (Set.toList collected)))

checkAnswer : Signal.Address Action -> Model -> List Html
checkAnswer address {guess, seed, state} =
  case state of
    (FinishedGame collected) ->  
      [ h2 [A.style [( "color", "#4A9")]] 
      [text (String.append "Där va alla ord slut! Du har hittat: " (collectedCharsAsCommaSeparatedString collected))]]
    (Guessing g wordlist collected) ->
      if correct guess then
        [ h2 [A.style [( "color", "#49A")]] [text "Rätt svar!"]
        , button [A.class "btn btn-success", buttonStyle, onClick address (NewWord state)]
        [ span [A.class "glyphicon glyphicon-thumbs-up"] []]]
      else
        [div [] []]

progress : Model -> Html
progress {guess, seed, state} =
  let n = wordList state |> List.length 
  in
      row_ [h2 [A.class "col-md-12" ] [text ("Du har " ++ (toString n) ++ " ord kvar!")]]

view : Signal.Address Action -> Model -> Html
view address model =
  container_
  [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , picture model
    , progress model
    , textControls address model
    , showGuess model
    , letterButtons address model
    , success address model 
    ]
