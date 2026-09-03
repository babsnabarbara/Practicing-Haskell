twice :: (a-> a) -> a -> a
twice f x = f(f x)

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a->b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs 

filter1 :: (a-> Bool) -> [a] -> [a]
filter1 f xs = [x | x <- xs, f x == True]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs)
   | f x = x : filter2 f xs
   | otherwise = filter2 f xs

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

(.:) :: (b -> c) -> (a -> b) -> (a -> c)
f .: g = \x -> f (g x)

any2 :: (a -> Bool) -> [a] -> Bool
any2 f xs = or [f x | x <- xs]

takeWhile2 :: (a-> Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (x:xs)
        | f x = x : takeWhile2 f xs
        | otherwise = []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f (x:xs) 
        | f x = dropWhile f xs
        | otherwise = x:xs

map_filter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
map_filter f p xs = map f (filter p xs)

map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldr (\x acc -> f x : acc) [] xs

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs
