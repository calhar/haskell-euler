import Data.List

-- Run through numbers from [2..10000]
-- for a, get the sum of a's divisors
-- check that a equals the sum of b's divisors
-- and that a /= b
--
-- then remove duplicates, concat, and sum
main = print $ sum $ concat $ nub $ getAmicables 10000

getAmicables x = [sort [a, b] | a <- [2..x],
                                b <- [sum (divisors a)],
                                a == sum (divisors b) && a /= b]

divisors b = filter (\y -> b `mod` y == 0) [1..b-1]
