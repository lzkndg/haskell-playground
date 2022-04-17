import Test.Hspec
import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member)

main :: IO ()
main = 
    hspec spec

spec :: Spec
spec = do
    describe "BinarySearchTree Implementation" $ do
        it "show (Set.fromList [1]) should equal \"Node Leaf 1 Leaf\"" $
            show (Set.fromList [1]) `shouldBe` "Node Leaf 1 Leaf"

        it "show (Set.insert 1 Set.empty) should equal \"Node Leaf 1 Leaf\"" $
            show (Set.insert 1 Set.empty) `shouldBe` "Node Leaf 1 Leaf"

        it "Set.fromList [1,2] should equal Set.insert 1 (Set.insert 2 Set.empty)" $
            show (Set.fromList [1,2]) `shouldBe` show (Set.insert 1 (Set.insert 2 Set.empty))

        it "Set.fromList [2,1] should equal Set.fromList [1,2]" $
            show (Set.fromList [1,2]) `shouldBe` show (Set.fromList [2,1])     

        it "t1 <> (t2 <> t3) should equal (t1 <> t2) <> t3" $
            show ((<>) t1 ((<>) t2 t3)) `shouldBe` show ((<>) ((<>) t1 t2) t3)
                where 
                    t1 = Set.insert 1 (Set.insert 2 Set.empty)
                    t2 = Set.fromList [2,3,1]
                    t3 = Set.fromList [5,7,9]
