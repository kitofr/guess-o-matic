module View where
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import String exposing (..)
import Set exposing (..)
import Types exposing (..)
import Seq exposing (..)
import Data exposing (..)
import Debug exposing (..)
import Svg
import Svg.Attributes as SvgA

row : List Html -> Html
row = div [ A.class "row", rowDistance ]

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

btn addr action classes what =
  button [ classes, buttonStyle ,onClick addr action ]
          what

primaryBtn label addr action =
  btn addr action (A.class "btn btn-primary") [text label]

charButton address c action =
  primaryBtn (bigAndSmall c) address (action c)

bigAndSmall : String -> String
bigAndSmall s =
    s ++ " " ++ (toLower s)


addButtons address answer action =
  List.map (\c -> charButton address (String.fromChar c) action) (uniqueChars answer)

iconButton addr action =
  btn addr action (A.class "btn glyphicon glyphicon-volume-up") []

addIconButtons address answer action =
  List.map (\c -> iconButton address (action (String.fromChar c))) (uniqueChars answer)

image : Guess -> String
image (_,question) =
  question.image

currentAnswer : Model -> String
currentAnswer {guess, state} =
  let (_,question) = guess
  in
      word question

hasMoreWords : Model -> Bool
hasMoreWords {guess, state} =
  let lst = wordList state
  in
      case lst of
        [] -> False
        otherwise -> True

picture : Model -> Html
picture {guess, state} =
  row [div [A.class "col-md-6"]
  [ img [ A.src (image guess)
  , A.width 300
  , A.height 300
  , A.style [("border","1px solid #AAA"),
             ("border-radius", "25px"),
             ("padding", "10px"),
             ("-webkit-box-shadow", "0 10px 6px -6px #777"),
             ("-moz-box-shadow", "0 10px 6px -6px #777"),
             ("box-shadow", "0 10px 6px -6px #777")
             ] ] [] ] ]

controlButton adr action icon =
  button [A.class ("btn btn-warning glyphicon " ++ icon),
          buttonStyle,
          onClick adr action ] []

textControls address model =
  row [ div [A.class "col-md-4" ]
      [ controlButton address Reset "glyphicon-refresh"
      , controlButton address Backspace "glyphicon-erase"]]

disabledButton address ch =
  let t = String.fromChar ch
  in
     button [ A.class "btn btn-disabled", buttonStyle, onClick address Backspace  ] [ text t ]

paddUpTo lst n =
  if List.length lst < n then
     paddUpTo (List.append lst ['_']) n
  else
    lst

answer : Guess -> String
answer (_, question) = (word question)

showGuess : Signal.Address Action -> Model -> Html
showGuess address {guess, state} =
  let answer' = Debug.watch "answer" (String.toList (answer guess))
      paddTo = (List.length answer')
      paddedGuess = Debug.watch "guess" (paddUpTo (String.toList (fst guess)) paddTo )
  in
  row [ div [A.class "col-md-4"]
           (List.map (disabledButton address) paddedGuess) ]

letterButtons : Signal.Address Action -> Model -> Html
letterButtons address model =
  row [ div [A.class "col-md-4"] (addButtons address (currentAnswer model) AddChar)]

soundButtons : Signal.Address Action -> Model -> Html
soundButtons address model =
  row [ div [A.class "col-md-4"] (addIconButtons address (currentAnswer model) PlayChar)]

success : Signal.Address Action -> Model -> Html
success address model =
  row [ div [A.class "col-md-4" ] (checkAnswer address model)]

collectedCharsAsCommaSeparatedString : Set Char -> String
collectedCharsAsCommaSeparatedString collected =
  (String.join "," (List.map String.fromChar (Set.toList collected)))

checkAnswer : Signal.Address Action -> Model -> List Html
checkAnswer address {guess, state} =
  case state of
    (FinishedGame collected score) ->
      [section []
          [h2 [A.style [( "color", "#4A9")]]
            [text ("Där va alla ord slut! " ++ (collectedCharsAsCommaSeparatedString collected))]]
          ]
    (Guessing g wordlist collected score) ->
      if correct guess then
        [ button [A.class "btn btn-success", buttonStyle, onClick address (NewWord state)]
        [ span [A.class "glyphicon glyphicon-thumbs-up"] []]]
      else
        [div [] []]

progress : Model -> Html
progress {guess, state} =
  let n = wordList state |> List.length |> Basics.toFloat
      tot = alternatives |> List.length |> Basics.toFloat
      total = 300.0
      complete = ((tot-n) / tot) * total
      width = (toString total)
  in
      Svg.svg
        [ SvgA.width width, SvgA.height "20", SvgA.viewBox ("0 0 " ++ width ++ " 20") ]
        [ Svg.rect [ SvgA.fill "#AAA", SvgA.x "0", SvgA.y "0", SvgA.width width, SvgA.height "20", SvgA.rx "5", SvgA.ry "5" ] [],
          Svg.rect [ SvgA.fill "#8C8", SvgA.x "0", SvgA.y "0", SvgA.width (toString complete), SvgA.height "20", SvgA.rx "5", SvgA.ry "5" ] []
        ]

score : Model -> Html
score {guess, state} =
   let score = (currentScore state)
       fontX = (toString (85 - ((score // 10) * 12)))
   in
    row [
      div [A.class "col-md-6"]
        [
          Svg.svg
            [ SvgA.width "100", SvgA.height "100", SvgA.viewBox "0 0 200 200"]
            [ Svg.polygon [ SvgA.fill "#DD7",
                SvgA.points "100,10 40,198 190,78 10,78 160,198" ] []
              ,Svg.text' [SvgA.fontSize "45", SvgA.x fontX, SvgA.y "130", SvgA.fill "blue"] [Svg.text (toString score)] ]]]

view : Signal.Address Action -> Signal.Address Action -> Model -> Html
view charBoxAddress address model =
  container_
  [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , picture model
    , score model
    , progress model
    , textControls address model
    , showGuess address model
    , letterButtons address model
    , soundButtons charBoxAddress model
    , success address model
    ]
