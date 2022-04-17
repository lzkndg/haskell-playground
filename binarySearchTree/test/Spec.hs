import Test.Hspec
import qualified BinarySearchTree
    as Set (T, empty, insert, fromList, toList, member)

main :: IO ()
main = hspec spec
    
spec :: Spec
spec = do
    describe "BinarySearchTree Implementation" $ do
        it "show (Set.fromList [1]) should equal \"Node Leaf 1 Leaf\"" $
            show (Set.fromList [1]) `shouldBe` "Node Leaf 1 Leaf"
        it "show (Set.insert 1 Set.empty) should equal \"Node Leaf 1 Leaf\"" $
            show (Set.insert 1 Set.empty) `shouldBe` "Node Leaf 1 Leaf"
        -- it 