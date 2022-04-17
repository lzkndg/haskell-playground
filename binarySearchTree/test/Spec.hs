import Test.Hspec
import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member, merge)

main :: IO ()
main = 
    hspec spec

spec :: Spec
spec = do
    describe "BinarySearchTree Implementation" $ do
        it "show (Set.fromList [1]) equals \"Node Leaf 1 Leaf\"" $
            show (Set.fromList [1]) `shouldBe` "Node Leaf 1 Leaf"

        it "show (Set.insert 1 Set.empty) equals \"Node Leaf 1 Leaf\"" $
            show (Set.insert 1 Set.empty) `shouldBe` "Node Leaf 1 Leaf"

        it "Set.fromList [1,2] equals Set.insert 1 (Set.insert 2 Set.empty)" $
            show (Set.fromList [1,2]) `shouldBe` show (Set.insert 1 (Set.insert 2 Set.empty))

        it "Set.toList (Set.fromList [2,1]) equals Set.toList (Set.fromList [1,2])" $
            show (Set.toList (Set.fromList [1,2])) `shouldBe` show (Set.toList (Set.fromList [2,1]))     

        it "t1 equals Node (Node Leaf 1 Leaf) 2 Leaf" $
            show t1 `shouldBe` "Node (Node Leaf 1 Leaf) 2 Leaf"

        it "t2 equals Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)" $
            show t2 `shouldBe` "Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)"

        it "show (t1 <> t2) == show t2" $
            show (t1 <> t2) `shouldBe` show t2

        it "show (t1 <> (t2 <> t3)) should equal show((t1 <> t2) <> t3)" $
            show ((<>) t1 ((<>) t2 t3)) `shouldBe` show ((<>) ((<>) t1 t2) t3)

        it "Set.toList t3 should be sorted" $
            isSorted (Set.toList t3) `shouldBe` True
    
        it "should merge equally two ways with empty tree" $
            Set.toList(Set.merge t3 Set.empty) `shouldBe` Set.toList(Set.merge Set.empty t3)

    where 
        t1 = Set.insert 1 (Set.insert 2 Set.empty)
        t2 = Set.fromList [1,3,2]
        t3 = Set.fromList [5,7,9]
        t4 = Set.merge t3 Set.empty
        t5 = Set.merge Set.empty t3

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)