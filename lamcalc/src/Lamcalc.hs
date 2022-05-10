module Lamcalc
    ( reduce
    ) where

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

reduce :: a
reduce = undefined

freeVars :: Term -> [a]
freeVars (Var v) = []
freeVars (Abs v (Var x)) = []
freeVars (Abs v (Abs id term)) = undefined
freeVars (Abs v (App term1 term2)) = undefined 
freeVars (App term1 term2) = undefined

-- isBetaRedex :: Term -> Bool 
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

{-

Var x
Abs (id) (term)
App (abs _ _ ) (Var)


Free variable:
Alle Var in Term t, die nicht in ( Abs >>Var<< (Term t) ) vorkommen
-}