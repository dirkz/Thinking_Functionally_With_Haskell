import Prelude as P
import Control.Monad (forM_)

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : Main.take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = Main.drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs
  | n < 0 = Main.splitAt 0 xs
  | otherwise = loop [] xs n
  where
    loop :: [a] -> [a] -> Int -> ([a], [a])
    loop ns ms 0 = (ns, ms)
    loop ns [] n = (ns, [])
    loop ns (x:xs) n  = loop (ns ++ [x]) xs (n - 1)

check = map (\n -> (Main.splitAt n input) == (P.splitAt n input)) ns
  where
    ns = [-2..10]
    input = [1..5]
    compare n = (Main.splitAt n input) == (P.splitAt n input)

main = forM_ (check) (P.putStrLn . show)
