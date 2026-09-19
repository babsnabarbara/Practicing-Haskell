sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

primes :: [Integer]
primes = sieve [2..]

twin :: (Integer, Integer) -> Bool
twin (x,y) = y == x+2

twins :: [(Integer, Integer)]
twins = filter twin (zip primes (tail primes))