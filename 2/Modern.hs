module Modern where

import Data.Char (toUpper)

modernise :: String -> String
modernise = unwords . map cap . words
    where
      cap :: String -> String
      cap [] = []
      cap (x:xs) = toUpper x : xs
