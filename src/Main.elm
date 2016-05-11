import Platform.Cmd as Cmd exposing (Cmd)
import Html.App as Html exposing (..)
import String exposing (..)
import View exposing (view)
import Types exposing (..)

{--
TODO
  - Styling
  - Animations
  - indicate overextending
  - Random chars in guess
  - add cheets (3 sec show next letter etc)
  - more sounds http://soundbible.com/free-sound-effects-1.html
  - Play word initially
--}

main : Program Never
main =
  Html.program { init = init
               , view = view
               , update = update
               , subscriptions = \_ -> Sub.none
               }

--charBox : Signal.Mailbox Msg
--charBox =
--  Signal.mailbox (PlayChar "")

--port playChar : String -> Cmd msg
--port playChar =
--  Signal.map
--    (\x -> case x of
--      (PlayChar ch) -> ch
--      otherwise -> "")
--  charBox.signal
--
--port correct : Bool -> Cmd msg
--port correct = Signal.map (\m -> Types.correct m.guess) app.model

nextWord : Model -> GameState -> Model
nextWord model state' =
  case state' of
    (FinishedGame collected score) -> { guess = model.guess , state = state' }
    (Guessing g l collected score) -> { guess = g, state = state' }

init : (Model, Cmd Msg)
init =
  let state' = Types.initialState
      guess' = Types.createGuess "" state'
  in
    ({ guess = guess', state = state' }, Cmd.none)

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddChar ch ->
      (addChar ch model, Cmd.none)

    Reset ->
      let (_,q) = model.guess
      in
      ({ model | guess = ("", q) }, Cmd.none)

    Backspace ->
      (backspace model, Cmd.none)

    NewWord state ->
      (nextWord model state, Cmd.none)

    PlayChar ch -> (model, Cmd.none)
