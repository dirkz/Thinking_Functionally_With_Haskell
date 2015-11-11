-- head . filter p

first :: (a -> Bool) -> [a] -> a
first p xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first p (tail xs)
  where x = head xs

-- head . filter p . map f

first2 :: (a -> Bool) -> (a -> b) -> [a] -> b
first2 p f xs
  | null xs = error "Empty list"
  | p x = f x
  | otherwise = first2 p f (tail xs)
  where x = head xs

first3 :: (b -> Bool) -> (a -> b) -> [a] -> b
first3 p f xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first3 p f (tail xs)
  where x = f (head xs)
