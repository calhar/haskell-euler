import Data.List

-- brute forcing it
-- 
-- a is all primes lower than v
-- b is all primes s.t. a < b < v
-- c is b + (b - a) < v
--
-- check if c is prime and if a, b, c are permutations
main = print $ filter (\(a, b, c) -> a == 1487) $ primePerms 10000

get_primes a b = filter isPrime [a..b]

isPrime x = null $ filter (\y -> x `mod` y == 0)
              $ takeWhile (\y -> y*y <=x) [2..]

isPermute x y = sort (show x) == sort (show y)

primePerms v = [(a, b, c) | a <- get_primes 1 v,
                            b <- get_primes (a+1) v,
                            c <- takeWhile (<v) [b + (b - a)],
                            isPrime c && isPermute a b && isPermute a c]
