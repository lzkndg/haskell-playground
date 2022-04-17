module Main where

import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member)

t1 :: Set.T Integer
t1 = Set.insert 1 (Set.insert 2 Set.empty)
t2 :: Set.T Integer
t2 = Set.fromList [2,3,1]
t3 :: Set.T Integer
t3 = Set.fromList [5,7,9]

-- >>> Set.member 3 t1

-- >>> t1 == t2

-- >>> Set.insert 3 t1

-- >>> show t1

-- >>> show t2

-- >>> show t3

-- >>> show (t1 <> t2) == show t2

-- >>> show ((<>) t1 ((<>) t2 t3))

-- >>> show ((<>) ((<>) t1 t2) t3)

-- >>> show ((<>) ((<>) t1 t2) t3) == show ((<>) t1 ((<>) t2 t3))

-- >>> Set.toList t3

-- >>> show (Set.merge t3 Set.empty) == show (Set.merge Set.empty t3)

main :: IO ()
main = return ()
