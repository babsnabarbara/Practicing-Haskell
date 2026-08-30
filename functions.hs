import Distribution.Simple.Utils (xargs)
abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else if n == 0 then 0 else 1 

abs1 :: Int -> Int
abs1 n | n>0 = n
         | otherwise = -n

signum1 :: Int -> Int
signum1 n | n < 0 = -1
            | n > 0 = 1
            | otherwise = 0

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- lazy evaluation
(&&&) :: Bool -> Bool -> Bool   
True &&& b = b
False &&& _ = False


head :: [a] -> a
head (x:_) = x

tail1:: [a] -> [a]
tail1 (_:xs) = xs

add :: Int -> Int -> Int
add x y = x + y

--lambda version
add1 :: Int -> (Int -> Int)
add1 = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map f [0..n-1]
    where
        f x = x * 2 + 1

--lambda version 
odds2 :: Int -> [Int]
odds2 n = map (\x -> x*2 + 1) [0..n-1]

--conditional version
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

--guarded equations
safetail1 :: [a] -> [a]
safetail1 xs | null xs = []
             | otherwise = tail xs

-- pattern matching
safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 (_:xs) = xs
    
 --three versions of || operator
(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

(|||) :: Bool -> Bool -> Bool
True ||| True = True
True ||| False = True
False ||| True = True
_ ||| _ = False

(||||) :: Bool -> Bool -> Bool
_ |||| True = True
True |||| _ = True
False |||| False = False

(&&&&) :: Bool -> Bool -> Bool
b1 &&&& b2 = if b1 == True then (if b2 == True then True else False) else False


(&&&&&) :: Bool -> Bool -> Bool
b1 &&&&& b2 = if b1 then b2 else False

