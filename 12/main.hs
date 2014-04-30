import Data.List

main = print $ head $ factorDigits 500

-- divisor function tells us that
-- divisors(val) = sum (a_i + 1)
-- where a_i is the power of the ith prime factor
--
-- We can calculate the nth triangle number using n * (n+1) / 2
-- Given this, the prime factors of the nth triangle number
-- equals the concatenation of the prime factors of n and (n+1)
--
-- remove a 2, count digits, add 1 to everything and take product
--
-- Then filter for divisors > 500
--
factorDigits dig = filter (\t -> (snd t) > dig) $ map (\t -> (fst t, product (snd t))) triangleFacs

triangleFacs = 
              let sieve [] = []
                  sieve (p:xs) = p : sieve[x|x <- xs, x `mod` p > 0]
                  primes = sieve (2 : [3, 5 ..])
                  countFacs list = length(list)
              in [((n * (n+1)) `div` 2, map (+1) (map countFacs (group (delete 2 (sort ((primeFac n primes) ++ (primeFac (n+1) primes))))))) | n <- [1..]]

primeFac 0 _ = []
primeFac 1 _ = []
primeFac val (p:xs) = if val `mod` p == 0
                      then p : primeFac (val `div` p) (p:xs)
                      else primeFac val xs

