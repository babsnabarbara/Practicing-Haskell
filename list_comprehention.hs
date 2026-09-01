myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x<-xs]

evenList :: [Int] -> [Int]
evenList xs = [x | x <- xs, even x]

evenToX :: Int -> [Int]
evenToX x = [x | x <- [1..x], even x]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x]

pairing :: [a] -> [(a,a)]
pairing xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <=y | (x, y) <- pairing xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

count :: Char -> String -> Int
count x xs = length[x' | x' <- xs, x == x']

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y<- [1..n], z<- [1..n], (x^2 + y^2) == z^2]

perfects :: Int -> [Int]
perfects n = [ n | n <- [1..n], sum (factors n) - n == n]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]