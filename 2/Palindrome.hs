import Data.Char (isAlpha, toLower)

reduce :: String -> String
reduce = map toLower . filter isAlpha

palindrome :: String -> String
palindrome = reverse . reduce

isPalindrome :: String -> Bool
isPalindrome s = reduce s == palindrome s

answer :: String -> String
answer s
  | isPalindrome s = "Yes!"
  | otherwise = "No!"

main = do
  txt <- getLine
  putStrLn $ answer txt
