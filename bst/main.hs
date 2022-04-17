import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member)

t1 :: Set.T Integer
t1 = Set.insert 1 (Set.insert 2 Set.empty)

t2 :: Set.T Integer
t2 = Set.fromList [2,3,1]
t3 = Set.fromList [3,1,2]

-- >>> Set.member 3 t1
-- False

-- >>> t1 == t2
-- False

-- >>> Set.insert 3 t1
-- Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- >>> show t1
-- "Node (Node Leaf 1 Leaf) 2 Leaf"

-- >>> show t2
-- "Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf"

-- >>> show t3
-- "Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf"
