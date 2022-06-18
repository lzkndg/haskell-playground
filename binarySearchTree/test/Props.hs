{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member, merge)
import Test.QuickCheck

propElementInTree :: Show a => a -> Bool
propElementInTree x = show (Set.fromList [x]) == ("Node Leaf " ++ show x ++ " Leaf")
    || show (Set.fromList[x]) == ("Node Leaf (" ++ show x ++ ") Leaf") 

-- prop> \x -> show (Set.fromList[x]) == ("Node Leaf " ++ show x ++ " Leaf") || show (Set.fromList[x]) == ("Node Leaf (" ++ show x ++ ") Leaf")
-- +++ OK, passed 100 tests.

-- propElementMemberOfTree :: Int -> Bool 
-- propElementMemberOfTree x = Set.member set x
--     where set = Set.fromList [x]

propToListIsNotEmpty :: Bool 
propToListIsNotEmpty = Set.toList empty /= []
    where empty = Set.fromList ['a']