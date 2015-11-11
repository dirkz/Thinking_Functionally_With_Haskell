disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint (x:xs) (y:ys)
  | x == y = False
  | x < y = disjoint (dropWhile (<= y) xs) ys
  | x > y = disjoint xs (dropWhile (<= x) ys)
