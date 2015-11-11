class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
--cross (f, g) = fork (f . fst, g . snd)
cross = uncurry bimap

instance Bifunctor Either where
  bimap f g (Left a) = Left $ f a
  bimap f g (Right c) = Right $ g c
