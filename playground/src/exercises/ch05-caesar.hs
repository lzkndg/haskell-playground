module Caesar where

import Data.Char
import Prelude

lowers :: String -> Int
lowers xs = length [x | x <- xs, isAsciiLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

let2int :: Char -> Int
let2int c 
    | isUpper c = ord c - ord 'A' 
    | isLower c = ord c - ord 'A' - 6
    | otherwise = undefined

int2let :: Int -> Char
int2let n
    | n >= 0 && n <= 25 = chr (ord 'A' + n)
    | n > 25 && n < 52 = chr (ord 'A' + n + 6)
    | otherwise = undefined

shift :: Int -> Char -> Char
shift n c
    | isAlpha c = int2let((let2int c + n) `mod` 52)
    | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n xs = [shift n c | c <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
        0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
        6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

minElem :: Ord a => [a] -> a
minElem [] = undefined
minElem [x] = x
minElem (x:xs) = min x (minElem xs)

