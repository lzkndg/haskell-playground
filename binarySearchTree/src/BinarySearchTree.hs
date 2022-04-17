module BinarySearchTree
( T, empty, insert, fromList, toList, member, merge )
where

data T a = Leaf | Node (T a) a (T a)
    deriving Show

instance Eq a => Eq (T a) where
    Leaf == Leaf = True
    Node ta1 r1 tb1 == Node ta2 r2 tb2 = ta1 == tb1 && r1 == r2 && tb1 == tb2
    _ == _ = False

instance Functor T where
    fmap _ Leaf = Leaf
    fmap g (Node ta r tb) = Node (fmap g ta) (g r) (fmap g tb)

instance Ord a => Semigroup (T a) where
    (<>) = merge

-- instance Monoid (T a) where
--     x <> mempty = x
--     mempty <> x = x
--     x <> (y <> z) = (x <> y) <> z

toList :: T a -> [a]
toList Leaf = []
toList (Node ta r tb) = toList ta ++ [r] ++ toList tb

member :: Ord a => a -> T a -> Bool
member e Leaf = False
member e (Node ta r tb) = e == r || member e ta || member e tb

empty :: T a
empty = Leaf

insert :: Ord a => a -> T a -> T a
insert e Leaf = Node Leaf e Leaf
insert e (Node ta r tb)
    | e < r = Node (insert e ta) r tb
    | e > r = Node ta r (insert e tb)
    | otherwise = Node ta r tb

fromList :: Ord a => [a] -> T a
fromList xs = foldr insert Leaf (qsort xs)
    where
        qsort [] = []
        qsort (x:xs) = 
            qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

merge :: Ord a => T a -> T a -> T a
merge Leaf Leaf = Leaf
merge ta Leaf = ta
merge Leaf tb = tb
merge ta tb = foldr insert tb taAsList
    where taAsList = toList ta