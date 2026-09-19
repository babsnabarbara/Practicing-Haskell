import System.IO

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x, y)

---------------------------------

getLine1 :: IO String
getLine1 = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine1
                   return (x:xs)

------------------------------------

putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 (x:xs) = do putChar x
                    putStr1 xs

-------------------------------------

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show(length xs))
            putStrLn " characters"

--------------------------------------


