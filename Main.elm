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
  - Sounds
  - Indicate how many letters
  - Don't repeat words (to often at least)
  - Count correct words
--}

main =
  start { model = init 
        , view = view
        , update = update }

alternatives = 
  [ { word = "APA", image = "http://www.skolbilder.com/Malarbild-apa-dm17524.jpg" }
  , { word = "MUS", image = "http://www.malarbok.nu/images/collection/169/large.jpg" }
  , { word = "KO", image = "http://ian.umces.edu/imagelibrary/albums/userpics/12789/normal_ian-symbol-bos-primigenius-cow-1.png" }
  , { word = "HUS", image = "http://www.featurepics.com/FI/Thumb300/20110927/Cartoon-House-2009748.jpg" }
  , { word = "BUSS", image = "http://cdn.topvectors.com/img/605/600x0/4:3/cartoon-school-bus-vector-illustrator_1330497143_large.jpg" }
  , { word = "RÅTTA", image = "http://hdwallpaperslovely.com/wp-content/gallery/rat-cartoon-images/rats.jpg" }
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
  button [ A.class "btn btn-primary" , onClick addr x ]
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

picture model =
  row_ [div [A.class "col-md-4"] 
  [ img [ A.src (.image (nth model.currentIndex alternatives defaultAlternative))
  , A.width 200
  , A.height 200
  , A.style [("border","2px solid black")] ] [] ] ]

textControls address model =
  row_ [ div [A.class "col-md-4"]
      [ button [ A.class "btn btn-warning", onClick address Reset ] 
              [ span [A.class "glyphicon glyphicon-backward"] [ ] ]
      , button [ A.class "btn btn-warning", onClick address Backspace ] 
              [ span [A.class "glyphicon glyphicon-step-backward"] [ ] ]]]

guess model =
  row_ [ h1 [A.class "col-md-4"] [text (toString model.guess) ]]

letterButtons address model =
  row_ [ div [A.class "col-md-4"] (addButtons address model.answer)]

success address model =
  row_ [ div [A.class "col-md-4" ] (checkAnswer address model)]

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
    , picture model
    , textControls address model
    , guess model
    , letterButtons address model
    , success address model 
    ]

type Action = 
  AddChar String
  | Reset
  | Backspace
  | NewWord

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
    Reset -> { model | guess <- "" }
    Backspace -> { model | guess <- String.dropRight 1 model.guess }
    NewWord -> chooseNewWord model
