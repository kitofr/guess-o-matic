import StartApp.Simple exposing (start)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Random exposing (..)
import Maybe exposing (..)
import Debug exposing (..)
import View exposing (view)
import Types exposing (..)
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

nextWord : Model -> GameState -> Model
nextWord model state' =
  case state' of
    (FinishedGame collected score) -> { guess = model.guess , state = state' }
    (Guessing g l collected score) -> { guess = g, state = state' }
        
init : Model
init =
  let state' = Types.initialState
      guess' = Types.createGuess "" state'
  in
    { guess = guess', state = state' }

addChar : String -> Model -> Model
addChar ch {guess, state} =
  let (g, q) = guess
      g' = (String.append g ch, q)
  in
      { guess = g', state = (Types.addGuess g' state) }

backspace : Model -> Model
backspace {guess, state} =
  let (g, q) = guess
      g' = String.dropRight 1 g
  in
      { guess = (g', q), state = (Types.addGuess (g',q) state) }

update : Action -> Model -> Model
update action model =
  case action of
    AddChar ch -> 
      addChar ch model

    Reset -> 
      let (_,q) = model.guess
      in
      { model | guess = ("", q) }

    Backspace -> 
      backspace model
      
    NewWord state -> nextWord model state
