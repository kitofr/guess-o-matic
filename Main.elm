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
  - Indicate how many letters (use _?)
  - indicate overextending
  - Show unique chars colleted
--}

main =
  start { model = init 
        , view = view
        , update = update }

nextWord : Model -> Model
nextWord model =
  let (wordIndex, seed') = Random.generate (Random.int 0 ((List.length alternatives) - 1)) model.seed
      rest = Maybe.withDefault [] (List.tail model.wordList)
      collected = Debug.watch "collected" (addChars (View.currentAnswer model) model.collectedChars )
      --_ = Debug.watch "wordList" (List.map (\c -> c.word) rest)
  in
  { model | 
      guess <- ""
      , seed <- seed'
      , wordList <- rest 
      , collectedChars <- collected
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
