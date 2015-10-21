import Html exposing (..)
import Html.Attributes exposing (src, width, height)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Random exposing (..)
import Maybe exposing (..)
import Debug exposing (..)

main =
  start { model = init 
        , view = view
        , update = update }

alternatives = 
  [ { word = "APA", image = "http://www.skolbilder.com/Malarbild-apa-dm17524.jpg" }
  , { word = "MUS", image = "http://www.malarbok.nu/images/collection/169/large.jpg" }
  ]

defaultAlternative =
  { word = "NOT FOUND", image = "http://www.404notfound.fr/assets/images/pages/img/androiddev101.jpg" }

type alias Model = { 
  answer : String, 
  guess : String,
  currentIndex : Int,
  seed : Seed 
}

chooseNewWord : Model -> Model
chooseNewWord model =
  let (wordIndex, seed') =
    Random.generate (Random.int 0 ((List.length alternatives) - 1)) model.seed
  in
    if model.currentIndex == wordIndex
       then 
        chooseNewWord { model | seed <- seed' }
        else
          let newWord = Debug.watch "new word" (.word (nth wordIndex alternatives { word = "NOT FOUND", image = ""}))
          in
          { model | 
            currentIndex <- Debug.watch "currentIndex" wordIndex
            , guess <- ""
            , answer <- newWord 
            , seed <- seed'
          }
        

nth n lst def =
  List.drop n lst |> List.head |> Maybe.withDefault def
  
init : Model
init =
  { answer = (.word (nth 0 alternatives { word = "APA", image = "http://www.skolbilder.com/Malarbild-apa-dm17524.jpg" } ))
  , currentIndex = 0
  , guess = "" 
  , seed = Random.initialSeed 12345
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
    [ div [] [ img [ src (.image (nth model.currentIndex alternatives defaultAlternative))
                     , width 100
                     , height 100] [] ]
    , div [] [ text (toString model.guess) ]
    , div [] [ text (toString model.answer) ]
    , div [] (addButtons address model.answer)
    , div [] [ button [ onClick address Reset ] [ text "Reset"]]
    , div [] [ button [ onClick address NewWord ] [ text "Generate new word"]]
    , div [] [ text (checkAnswer model)]]

type Action = 
  AddChar String
  | Reset
  | NewWord

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
    Reset -> init
    NewWord -> chooseNewWord model
