import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Html exposing (..)
import String exposing (..)
import Set exposing (..)
import List exposing (..)
import Random exposing (..)
import Maybe exposing (..)
import Debug exposing (..)
import Html exposing (..)
import View exposing (view)
import Types exposing (..)
import Seq exposing (..)
import Data exposing (..)

{--
TODO
  - indicate overextending
  - Styling
  - Random chars in guess
  - add cheets (3 sec show next letter etc)
  - more sounds http://soundbible.com/free-sound-effects-1.html
  - Play word initially
--}

app =
  StartApp.start { init = init
                  , view = (view charBox.address)
                  , update = update
                  , inputs = []
                  }

main : Signal Html
main =
  app.html

port tasks :Signal (Task Never ())
port tasks =
  app.tasks


charBox : Signal.Mailbox Action
charBox =
  Signal.mailbox (PlayChar "")

port playChar : Signal String
port playChar =
  Signal.map
    (\x -> case x of
      (PlayChar ch) -> ch
      otherwise -> "")
  charBox.signal

port correct : Signal Bool
port correct = Signal.map (\m -> Types.correct m.guess) app.model

nextWord : Model -> GameState -> Model
nextWord model state' =
  case state' of
    (FinishedGame collected score) -> { guess = model.guess , state = state' }
    (Guessing g l collected score) -> { guess = g, state = state' }

init : (Model, Effects Action)
init =
  let state' = Types.initialState
      guess' = Types.createGuess "" state'
  in
    ({ guess = guess', state = state' }, Effects.none)

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

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddChar ch ->
      (addChar ch model, Effects.none)

    Reset ->
      let (_,q) = model.guess
      in
      ({ model | guess = ("", q) }, Effects.none)

    Backspace ->
      (backspace model, Effects.none)

    NewWord state ->
      (nextWord model state, Effects.none)

    PlayChar ch -> (model, Effects.none)
