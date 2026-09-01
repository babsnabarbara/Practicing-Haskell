fac :: Int -> Int
fac n = product [1..n]

fac1 :: Int -> Int
fac1 0 = 1
fac1 n = n * fac1(n-1)

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (x:xs) = x * product1 xs

length1 :: [a] -> Int
length1 [] = 0
length1 (_:xs) = 1 + length1 xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1(x:xs) = reverse1 xs ++ [x]

zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y: ys) = (x, y) : zip1 xs ys


drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = []
drop1 x (_:xs) = drop1 (x-1) xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where  
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs   

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs:xss) = xs ++ concat xss

replicate1 :: Int -> a -> [a]
replicate1 0 xs = []
replicate1 n xs = xs : replicate1 (n-1) xs 

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x 
(_:xs) !!! n = xs !!! (n-1) 

elem1 :: Eq a => a -> [a] -> Bool
elem1 n [] = False
elem1 n (x:xs) = yes || elem1 n xs
    where
        yes = x == n

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = smaller : merge x' x''
    where
        smaller = if x <= y then x else y
        x' = if x <= y then xs else x:xs
        x'' = if x > y then ys else y:ys

halve :: [a] -> ([a], [a])
halve xs = (ys, zs)
        where 
            first_half_size = (length xs) `div` 2
            ys = take first_half_size xs
            zs = drop first_half_size xs


mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort ys) (mergesort zs) 
            where (ys, zs) = halve xs


insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) 
            | n <= x = n : x : xs
            | xs == [] = [x, n]
            | otherwise = [x] ++ insert n xs

insertionsort :: [Int] -> [Int]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)