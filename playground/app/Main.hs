module Main where

import Lib
import Data.Bits (Bits(xor))

main :: IO ()
main = someFunc

-- Aufgaben Probeprüfung FP 2022

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myConcat2 :: [[a]] -> [a]
myConcat2 xss = [ x | xs <- xss, x <- xs]

myConcat3 :: [[a]] -> [a]
myConcat3 = foldr (++) []

minElem :: Ord a => [a] -> a
minElem [] = undefined
minElem [x] = x
minElem (x:xs) = min x (minElem xs)

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(a, b) -> f a b

-- curry :: ((a, b) -> c) -> (a -> b -> c)
-- curry (a, b) = (\a -> (\b -> a b))

