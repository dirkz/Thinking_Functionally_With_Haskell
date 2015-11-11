-- Exercise G

type Date = (Int, Int, Int)

months = ["January", "February", "March", "April", "May", "June", "July", "August",
         "September", "October", "November", "December"]

suffix :: Int -> String
suffix n
  | n == 11 || n == 21 || n == 31 = "st"
  | n == 12 || n == 22 = "nd"
  | otherwise = "th"

showDate :: Date -> String
showDate (d, m, y) = show d ++ suffix d ++ " " ++ months!!(m - 1) ++ ", " ++ show y

-- Exercise H

type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum xs = xs ++ show (dsum xs)
  where
    dsum :: CIN -> Int
    dsum = sum . map getDigit

valid :: CIN -> Bool
valid xs = xs == addSum raw
  where raw = take 8 xs
