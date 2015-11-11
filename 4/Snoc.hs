import Prelude hiding (head, last)

data List a = Nil | Snoc (List a) a
  deriving (Show, Ord, Eq)

last :: List a -> a
last Nil = undefined
last (Snoc _ x) = x

head :: List a -> a
head Nil = undefined
head (Snoc Nil x) = x
head (Snoc xs _) = head xs

toList :: [a] -> List a
toList = process . reverse
  where
    process [] = Nil
    process (x:xs) = Snoc (process xs) x

fromList :: List a -> [a]
fromList = reverse . process
  where
    process Nil = []
    process (Snoc xs x) = x : process xs
