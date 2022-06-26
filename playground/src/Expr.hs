module Expr where

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap g Var x        = Var (g x)
    fmap g (Val n)      = Val (g n)
    fmap g (Add l r)    = Add (fmap g l) (fmap g r)

instance Applicative Expr where
    pure :: a -> Expr a
    pure = Val

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Val n <*> e = Val n
    Var g <*> e = fmap g e 
    (Add l r) <*> e = Add (l <*> e) (r <*> e)

instance Monad Expr where
    (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    f >>= (Val n) = Val n
    f >>= (Var x) = f x
    f >>= (Add l r) = Add (f >>= l) (f >>= r)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = (eval x) + (eval y)

-- (pure (\ x y -> [x] ++ [y]) <*> (Add (Val 1) (Var 'x')) <*> (Add (Val 2) (Var 'y')))

