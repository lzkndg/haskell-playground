module IO where

myGetLine :: IO [Char]
myGetLine = do
    x <- getChar 
    if x == '\n'
        then return []
        else
            do
            xs <- getLine
            return (x : xs)

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