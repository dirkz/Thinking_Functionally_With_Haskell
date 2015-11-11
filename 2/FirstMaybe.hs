first :: (a -> Bool) -> [a] -> Maybe a
first p xs =
  case ys of
    [] -> Nothing
    (y:ys) -> Just y
    where ys = filter p xs
