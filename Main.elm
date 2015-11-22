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
  - Sounds
  - indicate overextending
  - add cheets (3 sec show next letter etc)
  - show progress
--}

main =
  start { model = init 
        , view = view
        , update = update }

nextWord : Model -> Types.GameState -> Model
nextWord model state' =
  let (_, seed') = Random.generate (Random.int 0 ((List.length alternatives) - 1)) model.seed
      _ = Debug.watch "nextWord.state: " state'
  in
      case state' of
        (Types.FinishedGame collected) -> { guess = model.guess , seed = seed', state = state' }
        (Types.Guessing g l collected) -> { guess = g, seed = seed', state = state' }
        
init : Model
init =
  let seed = Random.initialSeed 12345
      state = Types.initialState
  in
      case state of
        (Types.FinishedGame c) ->  
          { guess = ("", { word = "DET BÖRJAR MED INGET?", image = ""})
          , seed = seed
          , state = state
          }
        (Types.Guessing g l c) -> { guess = g, seed = seed, state = state }

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
