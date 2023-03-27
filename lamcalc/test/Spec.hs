import Test.Hspec
import Lamcalc (Term (..), prettyPrint, reduce, nfin, freeVars, substitute)
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Custom Lambda Calculator Implementation" $ do
        it "should prettyPrint a Variable correctly" $
            prettyPrint (Var "a") `shouldBe` "a"
        it "should prettyPrint an Abstraction correctly" $
            prettyPrint (Abs "x" (Var "y")) `shouldBe` "(%x.y)"
        it "should prettyPrint an Application correctly" $
            prettyPrint (App (Abs "x" (Var "x")) (Var "a")) `shouldBe` "((%x.x) a)"
        it "should find nonfree variables with nfin" $
            nfin "x" (App (Abs "x" (Var "x")) (Var "a"))
        it "should not find free variables with nfin" $
            nfin "b" (App (Abs "x" (Var "x")) (Var "a")) `shouldBe` False
        it "should find freeVars in Application" $
            freeVars (App (Var "x") (Var "y")) `shouldBe` Set.fromList ["x","y"]
        it "should find freeVars in Abstraction" $
            freeVars (Abs "x" (App (Var "x") (Var "y"))) `shouldBe` Set.fromList["y"]
        it "should substitute correctly" $
            substitute ("y", (Var "z")) (Abs "x" (Var "y")) `shouldBe` Abs "x" (Var "z")