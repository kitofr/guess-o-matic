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
type alias WordList = List String

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

initialState = (Guessing ("foo", "foo") ["foo", "bar"] Set.empty)

updateCollected set word = List.foldr (\c a-> Set.insert c a) set (String.toList word)

correct (guess, answer) = guess == answer

addGuess : Guess -> GameState -> GameState
addGuess guess state = 
  let lst = wordList state
      collected' = updateCollected (collected state) (fst guess)
  in
    if | correct guess -> 
          case lst of 
            [] -> FinishedGame collected'
            h::t -> Guessing ("", h) t collected'
       | otherwise -> Guessing guess lst (collected state)
