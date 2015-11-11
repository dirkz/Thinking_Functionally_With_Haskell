module Exercisc35E where

import Prelude hiding (floor, sqrt)
import Control.Monad (forM, forM_, when)

leq :: Integer -> Float -> Bool
leq = (<=) . fromInteger

lt :: Float -> Integer -> Bool
lt x y = (x < fromInteger y)

type Interval = (Integer, Integer)

floor :: Float -> Integer
floor x = fst (until unit (shrink x) (bound x))
    where
      unit (m, n) = (m + 1 == n)

      shrink :: Float -> Interval -> Interval
      shrink x (m, n) = if p `leq` x then (p, n) else (m, p)
        where p = choose (m, n)

      choose :: Interval -> Integer
      choose (m, n) = (m + n) `div` 2

      bound :: Float -> Interval
      bound x = (lower x, upper x)

      lower :: Float -> Integer
      lower x = until (`leq` x) (* 2) (-1)

      upper :: Float -> Integer
      upper x = until (x `lt`) (* 2) 1

isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
    where
      unit (m, n) = (m + 1 == n)

      shrink :: Float -> Interval -> Interval
      shrink x (m, n) = if (p * p) `leq` x then (p, n) else (m, p)
        where p = choose (m, n)

      choose :: Interval -> Integer
      choose (m, n) = (m + n) `div` 2

      bound :: Float -> Interval
      bound x = (0, until above (*2) 1)
        where
          above :: Integer -> Bool
          above n = fromInteger (n * n) > x

sqrt :: Float -> Float
sqrt x = until goodEnough improve x
  where
    epsilon = 0.000001
    goodEnough y = abs (y * y - x) < epsilon * x
    improve y = (y + x/y) / 2

main = do
  -- isqrt
  let bs = map (\x -> (x, isqrt x == floor (sqrt x))) [0..100]
  forM_ bs (\(x, success) ->
    when (not success) $ putStrLn ("isqrt error with " ++ (show x)))
