module Types where
import Random exposing (Seed)
import Set exposing (..)

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

