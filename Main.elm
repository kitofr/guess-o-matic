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

nextWord : Model -> Types.GameState -> Model
nextWord model state' =
  let (_, seed') = Random.generate (Random.int 0 ((List.length alternatives) - 1)) model.seed
  in
      case state' of
        (Types.FinishedGame collected) -> { guess = model.guess , seed = seed', state = state' }
        (Types.Guessing g l collected) -> { guess = g, seed = seed', state = state' }
        
init : Model
init =
  let seed = Random.initialSeed 12345
  in
  { guess = ("", { word = "APA", image = ""})
  , seed = seed
  , state = Types.initialState
  }

addChar : String -> Model -> Model
addChar ch {guess, seed, state} =
  let (g, q) = guess
      g' = (String.append g ch, q)
  in
      { guess = g', seed = seed, state = (Types.addGuess g' state) }

backspace : Model -> Model
backspace {guess, seed, state} =
  let (g, q) = guess
      g' = String.dropRight 1 g
  in
      { guess = (g', q), seed = seed, state = (Types.addGuess (g',q) state) }

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> 
      addChar ch model

    Reset -> 
      let (_,q) = model.guess
      in
      { model | guess <- ("", q) }

    Backspace -> 
      backspace model
      
    NewWord state -> nextWord model state
