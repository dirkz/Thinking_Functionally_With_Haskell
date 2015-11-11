module Anagrams where

import Data.List (sort, intercalate)
import qualified Data.Map.Lazy as Map

type Word = String

extractN :: Int -> [Word] -> [Word]
extractN n =
  filter lengthN
    where
      lengthN :: String -> Bool
      lengthN word = length word == n

anagram :: Word -> Word
anagram = sort

anagramList :: [Word] -> [(Word, Word)]
anagramList = map (\w -> (anagram w, w))

anagramMap :: [(Word, Word)] -> Map.Map Word [Word]
anagramMap = Map.fromListWith (++) . map toList
  where
    toList :: (Word, Word) -> (Word, [Word])
    toList (label, w) = (label, [w])

anagrams :: Int -> [Word] -> String
anagrams n ws = foldl printer "" $ (Map.toList . anagramMap . anagramList . extractN n) ws
  where
    printer :: String -> (Word, [Word]) -> String
    printer s (w, ws) = s ++ "\n" ++ w ++ ": " ++ (intercalate ", " (sort ws))
