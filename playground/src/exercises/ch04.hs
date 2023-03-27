module Ch04 where

-- 1
-- halve :: [a] -> ([a], [a])
-- halve xs = (l1, l2) 
--     where 
--         l1 = take (length xs)/2 xs
--         l2 =  


-- 2
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 3

third3 :: [a] -> a
third3 [] = undefined
third3 [_] = undefined
third3 [_, _] = undefined
third3 (x:y:z:xs) = z

-- 3
safetail1 :: Eq a => [a] -> [a]
safetail1 xs = if xs == []
    then []
    else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
    | null xs = []
    | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- 4
(||) :: Bool -> Bool -> Bool 
-- True || True = True
-- True || False = True 
-- False || True = True
-- False || False = False 

-- False || False = False
-- _ || _ = True

False || b = b
True || _ = True 

-- 5
-- True && True = True 
-- _ && _ = False 

-- a && b = if a
--     then if b 
--         then True
--         else False 
--     else False

-- 6
-- a && b = if a
--     then b
--     else False

-- 7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- 8
luhnDouble :: Int -> Int
luhnDouble x 
    | (x * 2) > 9 = x * 2 - 9
    | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum `mod` 10 == 0
    where sum = luhnDouble a + b + luhnDouble c + d