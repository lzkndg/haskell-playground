module Ch03 where

-- What are the types of the following values?

-- ['a', 'b', 'c'] 
-- [Char]

-- ('a', 'b', 'c') 
-- (Char, Char, Char)

-- [(False, '0'), (True, '1')]
-- [(Bool, Char)]

-- ([False, True],['0', '1'])
-- ([Bool], [Char])

-- [tail, init, reverse]
-- [([a] -> [a])]


-- second xs = head (tail xs)
second :: [a] -> a
second (_:xs) = fst xs 


first (x:xs) = x

-- swap (x, y) = (y, x)
swap :: (a, b) -> (b, a)
swap = undefined

-- pair x y = (x, y)
pair :: a -> b -> (a, b)
pair = undefined

-- double x = x*2
double :: Num a => a -> a
double = undefined

-- palindrome xs = reverse xs == xs
palindrome :: Eq a => [a] -> Bool
palindrome = undefined

-- twice f x = f (f x)
twice :: (a -> a) -> a -> a
twice = undefined


myConcat xss = [x | x <- xs, xs <- xss]
