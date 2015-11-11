module Nat where

data Nat = Zero | Succ Nat

instance Eq (Nat) where
  Zero == Zero = True
  Zero == Succ n = False
  Succ n == Zero = False
  Succ n == Succ m = n == m

instance Show (Nat) where
  show Zero = "Zero"
  show (Succ n) = "Succ (" ++ show n ++ ")"

instance Num (Nat) where
  Zero + n = n
  n + Zero = n
  (Succ n) + (Succ m) = Succ (Succ (n + m))

  Zero - (Succ n) = error "No negative natural numbers"
  n - Zero = n
  (Succ m) - (Succ n) = m - n

  Zero * _ = Zero
  _ * Zero = Zero
  (Succ n) * m = m + (n * m)

  abs n = n

  negate Zero = Zero
  negate _ = error "Can't negate natural number"

  signum Zero = 0
  signum _ = 1

  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))

instance Ord (Nat) where
  compare Zero Zero = EQ
  compare Zero (Succ n) = LT
  compare (Succ n) Zero = GT
  compare (Succ n) (Succ m) = compare n m

divModN :: Nat -> Nat -> (Nat, Nat)
divModN Zero _ = (Zero, Zero)
divModN _ Zero = error "divide by zero"
divModN m n
  | m < n = (Zero, m)
  | (m == n) = (Succ Zero, Zero)
  | otherwise = (Succ q, r)
    where
      (q, r) = divModN (m - n) n
