module Ch05 where

-- 1
-- sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a, b) | a <- [0..x], b <- [0..y]]

-- 3
square :: Int -> [(Int, Int)]
square x = [(a, b) | (a, b) <- grid x x, a /= b ]

-- 4
replicate' :: Int -> a -> [a]
replicate' n elem = [elem | _ <- [1..n] ]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- 6
factors :: Int -> [Int]
factors x = [y | y <- [1..x-1], x `mod` y == 0 ] 

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

-- 7
-- [(x, y) | x <- [1,2], y <- [3,4]]
-- someTuples x y = [(x, y) | ]

-- 8

-- 9
scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [a*b | (a, b) <- zip xs ys] 