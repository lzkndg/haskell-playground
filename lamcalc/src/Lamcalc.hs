module Lamcalc
    ( reduce
    ) where

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

nfin :: Id -> Term -> Bool
nfin (Var x) (Var y) = x /= y
nfin (Var x) (Abs x term) = True
nfin (Var x) (Abs y term) = x /= y && nfin x (Abs y term)
nfin (Var x) (App t1 t2) = nfin x t1 && nfin x t2

freeVars :: Term -> [Id]
freeVars (Var v) = []
freeVars (Abs v (Var x)) = [x | nfin x (Abs v (Var x))]

freeVars (Abs v (Abs id term)) = [v | nfin v term] ++ 

freeVars (Abs v (App term1 term2)) = undefined

freeVars (App term1 term2) = undefined

reduce :: a
reduce = undefined

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

square -> no freevar
\x. x * x
Abs (id x) ( Term ((id x) * (id x)) )

timesX -> freevar x
\y. y*x
Abs (id y) ( Term ((id y) * (id x)) )

-}