act :: IO (Char,Char)
act = do x <-getChar
         y <- getChar
         return (x,y)

getMyLine :: IO [Char]
getMyLine = do x <- getChar
               if x == '\n' then return []
               else do xs <- getMyLine
                       return (x:xs)

strlen :: IO()
strlen = do putStr "Enter a string: "
            xs <- getMyLine
            putStr ("Length: " ++ show (length xs))
            