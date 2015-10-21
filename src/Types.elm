module Types where
import Random exposing (..)

type Action = 
  AddChar String
  | Reset
  | Backspace
  | NewWord

type alias Model = { 
  answer : String, 
  guess : String,
  currentIndex : Int,
  seed : Seed 
}

