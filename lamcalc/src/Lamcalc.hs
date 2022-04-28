module Lamcalc
    ( reduce
    ) where

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions
    | App Term Term  -- Applications

reduce :: IO ()
reduce = putStrLn "someFunc"