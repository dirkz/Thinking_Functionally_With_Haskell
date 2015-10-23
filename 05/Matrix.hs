module Matrix where

import Data.List (sort)

type Row a = [a]
type Col a = [a]
type Matrix a = [Row a]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
  where yss = cp xss

m1 :: Matrix Int
m1 = [[1,2,3],[4,5,6],[7,8,9]]

m2 :: Matrix Int
m2 = [[7,8,9],[4,5,6],[1,2,3]]

-- |Identity matrix
m3 :: Matrix Int
m3 = [[1,0,0],[0,1,0],[0,0,1]]

transpose :: Matrix a -> Matrix a
transpose [xs] = [[x] | x <- xs]
transpose (xs:xss) = zipWith (:) xs (transpose xss)

--
-- Exercise A
--

add1 :: Num a => Matrix a -> Matrix a
add1 = map (map (+1))

sumM :: Num a => Matrix a -> a
sumM = sum . map sum

addM :: Num a => Matrix a -> Matrix a -> Matrix a
addM = zipWith $ zipWith (+)

-- |Multiplies a matrix row with a col
multRowCol :: Num a => Row a -> Col a -> a
multRowCol m1 m2 = sum (zipWith (*) m1 m2)

-- |Multiplies a row with the given cols, resulting in a row
multRowCols :: Num a => Row a -> [Col a] -> Row a
multRowCols r = zipWith multRowCol (repeat r)

-- |Matrix multiplication
multM :: Num a => Matrix a -> Matrix a -> Matrix a
multM m1 m2 = map (flip multRowCols cols) m1
  where
    cols = transpose m2

--
-- Exercise B
--

-- [[],[]] -> 2,0 Matrix
-- [] -> 0,undefined Matrix

transpose' :: Matrix a -> Matrix a
transpose' [] = [[]]
transpose' (xs:xss) = zipWith (:) xs (transpose xss)

transpose'' :: Matrix a -> Matrix a
transpose'' ([]:xss) = []
transpose'' xss = map head xss : transpose'' (map tail xss)

--
-- Exercise C
--

-- any p = not . all (not p) -- true
-- any null = null . cp -- true
-- Both true

--
-- Exercise D
--

nodups :: (Ord a) => [a] -> Bool
nodups xs = and $ zipWith (/=) ys (tail ys)
  where ys = sort xs

--
-- Exercise E
--

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x:(nub $ filter (/= x) xs)

nub' :: (Ord a) => [a] -> [a]
nub' xs = nub'' $ sort xs
  where
    nub'' [] = []
    nub'' (x:xs) = x:(nub'' $ dropWhile (== x) xs)
