module Lamcalc
    ( reduce
    ) where

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

reduce :: a
reduce = undefined

-- freeVars :: Term -> [a]
-- freeVars (Var x) = []
-- freeVars (Abs (Var x) t) =  freeVars t
-- freeVars (App t1 t2) = freeVars t1 ++ freeVars t2 

isBetaRedex :: Term -> Bool 
isBetaRedex (Var id) = False
isBetaRedex (Abs id (Var v)) = False
-- isBetaRedex (Abs id (Term t)) = isBetaRedex t
isBetaRedex (App t1 t2) = isBetaRedex t1 || isBetaRedex t2
-- isBetaRedex (App (Abs x t) (Var id)) = True
isBetaRedex (Abs [] (Abs _ _)) = False
isBetaRedex (Abs [] (App _ _)) = False
isBetaRedex (Abs [_] (App _ _)) = False
isBetaRedex (Abs (_:_) (Abs _ _)) = False 
isBetaRedex (Abs (_:_) (App _ _)) = False

