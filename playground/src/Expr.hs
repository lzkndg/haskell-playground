module Expr where

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap g Var x        = Var (g x)
    fmap g (Val n)      = Val (g n)
    fmap g (Add l r)    = Add (fmap g l) (fmap g r)

instance Applicative where
    pure :: a -> Expr a
    pure = Val

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Var g <*> e = _
    Val n <*> e = _
    (Add l r) <*> e = _



-- (pure (\ x y -> [x] ++ [y]) <*> (Add (Val 1) (Var 'x')) <*> (Add (Val 2) (Var 'y')))

