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

instance Ord a => Monoid (T a) where
    mempty = Leaf

toList :: Ord a => T a -> [a]
toList Leaf = []
toList (Node ta r tb) = qsort (toList ta ++ [r] ++ toList tb)
    where
        qsort [] = []
        qsort (x:xs) =
            qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

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
fromList = foldr insert Leaf

merge :: Ord a => T a -> T a -> T a
merge Leaf Leaf = Leaf
merge ta Leaf = ta
merge Leaf tb = tb
merge ta tb = foldr insert tb taAsList
    where taAsList = toList ta