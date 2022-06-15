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