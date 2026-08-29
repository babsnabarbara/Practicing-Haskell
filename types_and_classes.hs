-- 1) What are the types of the following values?
--[Char] ['a', 'b', 'c'] 
--(Char, Char, Char) ('a', 'b', 'c')
-- [(Bool, Char)] [(False, '0'), (True, '1')]
-- ([Bool], [Char]) ([False, True], ['0', '1'])
-- [[a]->[a]] [tail, init, reverse]

--2) What are the types of the following functions?
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq  a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (x->x) -> x -> x
twice f x = f (f x)

