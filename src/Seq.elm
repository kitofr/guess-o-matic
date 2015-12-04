module Seq where
import String exposing (toUpper)
import Set exposing (insert, empty, toList)
import List exposing (drop, head, foldr, sort)
import Maybe exposing (withDefault)

nth n lst def =
  List.drop n lst |> List.head |> Maybe.withDefault def


uniqueChars string =
  List.foldr (\c a-> Set.insert c a) Set.empty 
  (String.toUpper string
    |> String.toList 
    |> List.sort)
  |> Set.toList


addChars word set =
  List.foldr (\c a-> Set.insert c a) set (String.toList word)

