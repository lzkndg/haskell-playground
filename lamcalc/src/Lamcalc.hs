module Lamcalc
    ( reduce
    ) where

import qualified Data.Set as Set

type Id = String
data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

nfin :: Id -> Term -> Bool
nfin x (Var y) = False                          -- single variable is always free
nfin x (Abs y term)                             -- abstraction of a variable could capture
    | x == y = True
    | otherwise = nfin x term
nfin x (App t1 t2) = nfin x t1 || nfin x t2     -- application must be recursively checked

-- >>> nfin "x" (App (Abs "x" (Var "x")) (Var "a"))
-- True

-- >>> nfin "b" (App (Abs "x" (Var "x")) (Var "a"))
-- False

-- >>> nfin "x" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
-- False

-- >>> nfin "a" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
-- False

freeVars :: Term -> Set.Set Id
freeVars (Var v) = Set.insert v Set.empty
freeVars (Abs x term) = Set.delete x (freeVars term)
freeVars (App term1 term2) = Set.union (freeVars term1) (freeVars term2)

-- >>> freeVars (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))

-- substitute :: (Id, Term) -> Term
-- substitute x (Var x) = Id tx 

isBetaRedex :: Term -> Bool
isBetaRedex = undefined
-- isBetaRedex (Var id) = False
-- isBetaRedex (Abs id (Var v)) = False
-- -- isBetaRedex (Abs id (Term t)) = isBetaRedex t
-- isBetaRedex (App t1 t2) = isBetaRedex t1 || isBetaRedex t2
-- -- isBetaRedex (App (Abs x t) (Var id)) = True
-- isBetaRedex (Abs [] (Abs _ _)) = False
-- isBetaRedex (Abs [] (App _ _)) = False
-- isBetaRedex (Abs [_] (App _ _)) = False
-- isBetaRedex (Abs (_:_) (Abs _ _)) = False 
-- isBetaRedex (Abs (_:_) (App _ _)) = False


reduce :: Term -> Term
reduce = undefined
-- reduce (Var a) = Var a
-- reduce Abs (Var a) (Term t) = 

-- >>> reduce (App (Abs "x" (Var "x")) (Var "a")) == (Var "a")

{-

Var x
Abs (id) (term)
App (abs _ _ ) (Var)


Free variable:
Alle Var in Term t, die nicht in ( Abs >>Var<< (Term t) ) vorkommen

square -> no freevar
\x. x * x
Abs (id x) ( Term ((id x) * (id x)) )

timesX -> freevar x
\y. y*x
Abs (id y) ( Term ((id y) * (id x)) )

-}

-- `(λx.x) a` reduces to `a`
-- >>> reduce (App (Abs "x" (Var "x")) (Var "a")) == (Var "a")
-- True

-- `(λx.x x) (λx.x)` reduces to `λx.x`
-- >>> reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == (Abs "x" (Var "x")) 
-- True

-- `(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y`
-- >>> reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) == (Abs "y1" (App (Var "y") (Var "y1")))
-- True

-- Note: The above test case is too brittle since its success depends on the choice of the fresh variable `y1` chosen to avoid variable capture.
-- To make the test more robust, it would be better to check that the two terms are alpha equivalent instead. 
-- Note: Checking if two terms are alpha equivalent is not part of the minimum goal.

-- `(λx.λy.x y) y` reduces to a term that is alpha equivalent to `λz.y z`
-- >>> alphaEq (reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))) (Abs "z" (App (Var "y") (Var "z")))
-- True
