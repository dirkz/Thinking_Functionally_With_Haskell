import Data.List (sort)

quads n = sort [(quad a b c d, (a,b,c,d))
    | a <- [1..n], b <- [a..n], c <- [a+1..n], d <- [c..n],
      cube a + cube b == cube c + cube d]
    where
      cube x = x^3
      quad a b c d = cube a + cube b
