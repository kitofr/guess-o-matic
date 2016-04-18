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
  FinishedGame CollectedChars Int
  | Guessing Guess WordList CollectedChars Int

type Action =
  AddChar String
  | PlayChar String
  | Reset
  | Backspace
  | NewWord GameState

type alias Model = {
  guess : Guess,
  state : GameState
}

wordList : GameState -> WordList
wordList state =
  case state of
    (FinishedGame _ _) -> []
    (Guessing _ list _ _) -> list

collected : GameState -> CollectedChars
collected state =
  case state of
    (FinishedGame collected _) -> collected
    (Guessing _ _ collected _) -> collected

initialState : GameState
initialState =
  case alternatives of
    h::t -> (Guessing ("", h) t Set.empty 0)
    _ -> FinishedGame Set.empty 0

updateCollected : Set Char -> Guess -> Set Char
updateCollected set (guess, _) = List.foldr (\c a-> Set.insert c a) set (String.toList guess)

correct : Guess -> Bool
correct (guess, question) = guess == (word question)

points : Guess -> Int
points (guess,{word,image}) =
  String.length word

word : Question -> String
word question = question.word

guess : GameState -> Guess
guess state =
  case state of
    (FinishedGame _ _) -> ("", { word = "", image = "" })
    (Guessing g _ _ _) -> g

createGuess : String -> GameState -> Guess
createGuess g state =
  let (_, q) = guess state
  in
      (g, q)

currentScore : GameState -> Int
currentScore state =
   case state of
     (FinishedGame _ score) -> score
     (Guessing _ _ _ score) -> score

addGuess : Guess -> GameState -> GameState
addGuess guess state =
  let lst = wordList state
      collected' = updateCollected (collected state) guess
      score = (currentScore state)
      score' = (points guess) + score
  in
    if correct guess then
          case lst of
            [] -> FinishedGame collected' score'
            h::t -> Guessing ("", h) t collected' score'
    else
      Guessing guess lst (collected state) score
