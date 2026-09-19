import Data.Char

-- Board utilities
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid b row num = (b !! (row-1)) >= num

move :: Board -> Int -> Int -> Board
move b row num = [adjust r n | (r, n) <- zip [1..5] b]
                    where
                        adjust r n = if r == row then n - num else n 

-- I/O utilities

newLine :: IO ()
newLine = putChar '\n'

stars :: Int -> String
stars n = concat(replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)
                

putBoard :: Board -> IO ()
putBoard = putRows 1
    where 
        putRows _ [] = return ()
        putRows n (x:xs) = do
            putRow n x 
            putRows (n+1) xs


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newLine
                     if isDigit x then
                        return (digitToInt x)
                      else 
                        do newLine 
                           putStrLn "ERROR: Invalid Digit!"
                           getDigit prompt


play :: Board -> Int -> IO ()
play board player = 
    do newLine 
       putBoard board
       if finished board then
          do newLine
             putStr "Player "
             putStr (show (next player))
             putStrLn " wins!"
        else
            do newLine
               putStr "Player"
               putStrLn (show player)
               r <- getDigit "Enter a row number: "
               n <- getDigit "Enter stars to remove: "
               if valid board r n then
                  play (move board r n) (next player)
                else 
                    do newLine 
                       putStrLn "ERROR: Invalid Move!"
                       play board player


                     
stargame :: IO ()
stargame = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

