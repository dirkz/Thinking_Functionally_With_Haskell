fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

unzip :: [(a,b)] -> ([a], [b])
unzip = fork (map fst, map snd)

cross :: (a -> b, a -> c) -> (a, a) -> (b, c)
cross (f, g) = fork (f . fst, g . snd)
