import StartApp.Simple exposing (start)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Random exposing (..)
import Maybe exposing (..)
import Debug exposing (..)
import View exposing (view)
import Types exposing (Action(..), Model)
import Seq exposing (..)
import Data exposing (..)


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
        
init : Model
init =
  { answer = (.word (nth 0 alternatives { word = "APA", image = "http://www.skolbilder.com/Malarbild-apa-dm17524.jpg" } ))
  , currentIndex = 0
  , guess = "" 
  , seed = Random.initialSeed 12345
  }

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
    Reset -> { model | guess <- "" }
    Backspace -> { model | guess <- String.dropRight 1 model.guess }
    NewWord -> chooseNewWord model
