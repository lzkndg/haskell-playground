module Countdown
    ( valid
    ) where
    
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x >= y
valid Mul _ _ = True
valid Div _ 0 = False
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
        where 
            brak (Val n) = show n
            brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
    y <- eval r, valid o x y]


myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    myPutStr xs

myPutStrLn :: String -> IO ()
myPutStrLn [] = do 
    putChar '\n'
    return ()
myPutStrLn (x:xs) = do
    putChar x
    myPutStrLn xs

strLen :: IO ()
strLen = do 
    putStr "Enter a string: "
    s <- getLine
    putStr "The string has "
    putStr (show (length s))
    putStrLn " characters." 