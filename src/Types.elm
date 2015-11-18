module Types where

import String exposing (..)
import Random exposing (Seed)
import Set exposing (..)
import List exposing (..)

type Action = 
  AddChar String
  | Reset
  | Backspace
  | NewWord

type alias Model = { 
  guess : String,
  seed : Seed,
  wordList :  List ( { word : String, image : String }),
  collectedChars : Set (Char)
}

type alias CollectedChars = Set Char
type alias Answer = String
type alias Guess = (String, Answer)
type alias Question = { word: String, image: String }
type alias WordList = List Question

type GameState =
  FinishedGame CollectedChars 
  | Guessing Guess WordList CollectedChars

wordList : GameState -> WordList
wordList state =
  case state of
    (FinishedGame _) -> []
    (Guessing _ list _) -> list

collected : GameState -> CollectedChars
collected state =
  case state of
    (FinishedGame collected) -> collected
    (Guessing _ _ collected) -> collected

initialState = (
    Guessing ("foo", "foo") 
    [{ word = "foo", image = "" }, { word = "bar", image = "" }] 
    Set.empty)

updateCollected : Set Char -> Guess -> Set Char
updateCollected set (guess, _) = List.foldr (\c a-> Set.insert c a) set (String.toList guess)

correct : Guess -> Bool
correct (guess, answer) = guess == answer

word : Question -> String
word question = question.word

addGuess : Guess -> GameState -> GameState
addGuess guess state = 
  let lst = wordList state
      collected' = updateCollected (collected state) guess
  in
    if | correct guess -> 
          case lst of 
            [] -> FinishedGame collected'
            h::t -> Guessing ("", (word h)) t collected'
       | otherwise -> Guessing guess lst (collected state)
