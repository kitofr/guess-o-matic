module Types where

import Data exposing (..)
import String exposing (..)
import Random exposing (Seed)
import Set exposing (..)
import List exposing (..)

type alias CollectedChars = Set Char
type alias Question = { word: String, image: String }
type alias Guess = (String, Question)
type alias WordList = List Question
type GameState =
  FinishedGame CollectedChars 
  | Guessing Guess WordList CollectedChars

type Action = 
  AddChar String
  | Reset
  | Backspace
  | NewWord GameState

type alias Model = { 
  guess : Guess,
  seed : Seed,
  state : GameState
}

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

initialState = 
  case alternatives of
    h::t -> (Guessing ("", h) t Set.empty)
    _ -> FinishedGame Set.empty
    

updateCollected : Set Char -> Guess -> Set Char
updateCollected set (guess, _) = List.foldr (\c a-> Set.insert c a) set (String.toList guess)

correct : Guess -> Bool
correct (guess, question) = guess == (word question)

word : Question -> String
word question = question.word

guess : GameState -> Guess
guess state =
  case state of
    (FinishedGame _) -> ("", { word = "", image = "" })
    (Guessing g _ _) -> g

createGuess : String -> GameState -> Guess
createGuess g state =
  let (_, q) = guess state
  in
      (g, q)
   
addGuess : Guess -> GameState -> GameState
addGuess guess state = 
  let lst = wordList state
      collected' = updateCollected (collected state) guess
  in
    if | correct guess -> 
          case lst of 
            [] -> FinishedGame collected'
            h::t -> Guessing ("", h) t collected'
       | otherwise -> Guessing guess lst (collected state)
