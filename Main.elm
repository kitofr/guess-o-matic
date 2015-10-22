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


{--
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
--}

nextWord : Model -> Model
nextWord model =
  let (wordIndex, seed') = Random.generate (Random.int 0 ((List.length alternatives) - 1)) model.seed
      rest = Maybe.withDefault [] (List.tail model.wordList)
      _ = Debug.watch "wordList" (List.map (\c -> c.word) rest)
  in
  { model | 
      guess <- ""
      , seed <- seed'
      , wordList <- rest 
      --, collectedChars <- Debug.watch "collected" addChars model.collectedChars model.answer
    }

generateWords seed = 
  alternatives
        
init : Model
init =
  let seed = Random.initialSeed 12345
  in
  { guess = "" 
  , seed = seed
  , wordList = generateWords seed
  , collectedChars = Set.empty
  }

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> { model | guess <- model.guess ++ ch }
    Reset -> { model | guess <- "" }
    Backspace -> { model | guess <- String.dropRight 1 model.guess }
    NewWord -> nextWord model
