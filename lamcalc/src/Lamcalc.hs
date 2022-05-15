module Lamcalc
    ( reduce
    ) where

import qualified Data.Set as Set

type Id = String
data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

instance Eq Term where
    Var a == Var b = a == b
    Abs x1 term1 == Abs x2 term2 = x1 == x2 && term1 == term2
    App term1 term2 == App term3 term4 = term1 == term3 && term2 == term4
    _ == _ = False

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
-- True
-- >>> nfin "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
-- True
-- >>> nfin "a" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
-- False

freeVars :: Term -> Set.Set Id
freeVars (Var v) = Set.insert v Set.empty
freeVars (Abs x term) = Set.delete x (freeVars term)
freeVars (App term1 (Var v))
    | nfin v term1 = freeVars term1
    | otherwise = Set.insert v (freeVars term1)
freeVars (App term1 term2) = Set.union (freeVars term1) (freeVars term2)

-- >>> freeVars (App (Var "x") (Var "y"))
-- fromList ["x","y"]
-- >>> freeVars (Abs "x" (App (Var "x") (Var "y")))
-- fromList ["y"]
-- >>> freeVars (Abs "y" (App (Var "x") (Var "y")))
-- fromList ["x"]
-- >>> freeVars (Abs "z" (App (Var "x") (Var "y")))
-- fromList ["x","y"]

-- >>> freeVars (Abs "y" (Abs "x" (App (Var "x") (Var "y"))))
-- fromList []

-- >>> freeVars (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
-- fromList []
-- >>> freeVars (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "z"))
-- fromList ["z"]

substitute :: (Id, Term) -> Term -> Term
-- substitute (x, tx) t replaces all free occurrences of the variable xi within the term t with the variable name tx, assuming the variable has a free occurence.
substitute (x, termx) (Var y)
    | x == y = termx
    | otherwise = Var y
substitute (x, termx) (Abs y term)
    | x /= y = Abs y (substitute (x, termx) term)
    | otherwise = undefined
substitute (x, termx) (App term1 term2) = App (substitute (x, termx) term1) (substitute (x, termx) term2)

-- >>> substitute ("y", (Var "z")) (Var "y") == Var "z"
-- True
-- >>> substitute ("y", (Var "z")) (Abs "x" (Var "y")) == Abs "x" (Var "z")
-- True

isBetaRedex :: Term -> Bool
isBetaRedex (App (Abs "x" term1) term2) = True
isBetaRedex _ = False

reduce :: Term -> Term
reduce term1 = undefined
-- reduce term1 | isBetaRedex term1 = 

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
