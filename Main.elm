import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import StartApp.Simple exposing (start)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Random exposing (..)
import Maybe exposing (..)
import Debug exposing (..)


{--
TODO 
  - Styling
  - Random chars in guess
  - Change word on success
    - show "button on correct"
  - More words
  - Extract view to separate file
  - Deploy to heroku
--}

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

uniqueChars string =
  List.foldr (\c a-> Set.insert c a) Set.empty 
  (String.toUpper string
    |> String.toList 
    |> List.sort)
  |> Set.toList

row_ : List Html -> Html
row_ = div [ A.class "row" ]

container_ : List Html -> Html
container_ = div [ A.class "container" ]

btnPrimary_ : String -> Signal.Address a -> a -> Html
btnPrimary_  label addr x =
  button
  [ A.class "btn btn-primary"
  , onClick addr x
  ]
  [ text label ]

stylesheet : String -> Html
stylesheet href =
  node "link"
  [ A.rel "stylesheet"
  , A.href href
  ] []

addButton address c =
    btnPrimary_ c address (AddChar c) 

addButtons address answer =
  List.map (\c -> addButton address (String.fromChar c)) (uniqueChars answer)

checkAnswer : Signal.Address Action -> Model -> List Html
checkAnswer address model  =
  if model.guess == model.answer then
     [ h1 [] [text "Rätt svar!"]
     , button [A.class "btn btn-success", onClick address NewWord]
       [ span [A.class "glyphicon glyphicon-random"] []]
       ]
  else
   [div [] []]

view address model =
  container_
  [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , row_ [div [A.class "col-md-4"] 
            [ img [ A.src (.image (nth model.currentIndex alternatives defaultAlternative))
                     , A.width 200
                     , A.height 200
                     , A.style [("border","2px solid black")] ] [] ] ]
    , row_ [ text (toString model.guess) ]
    , row_ (addButtons address model.answer)
    , row_ [ button [ A.class "btn btn-warning", onClick address Reset ] 
              [ span [A.class "glyphicon glyphicon-backward"] [ ] ]]
    , row_ [ div [A.class "col-md-4" ]
               (checkAnswer address model)]
    ]

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
