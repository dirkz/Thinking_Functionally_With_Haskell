module Song where

import Data.List (intercalate)
import Data.Char (toUpper, toLower)

song n =
  if n == 0 then ""
           else song (n-1) ++ "\n" ++ verse n

verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

numToString :: Int -> String
numToString 1 = "one"
numToString 2 = "two"
numToString 3 = "three"
numToString 4 = "four"
numToString 5 = "five"
numToString 6 = "six"
numToString 7 = "seven"
numToString 8 = "eight"
numToString 9 = "nine"
numToString 10 = "ten"

men :: Int -> String
men n
  | n > 1 = "men"
  | otherwise = "man"

menCount :: Int -> String
menCount n = numToString n ++ " " ++ men n

cap :: String -> String
cap [] = []
cap (x:xs) = toUpper x : xs

range :: Int -> [Int]
range n = take n $ iterate pred n

line1 n =
  (cap . numToString) n ++ " " ++ men n ++ " went to mow\n"

line2 _ = "Went to mow a meadow\n"

line3 n =
  let allMen = map menCount $ range n
      menString = intercalate ", " allMen
  in (cap menString) ++ " and his dog\n"

line4 = line2
