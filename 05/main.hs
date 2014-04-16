main = print (product(facList(20)))

-- Get all the prime factors of numbers [2..val]
-- using simple sieve of eratothenes
facList val = 
            let vals = (reverse [2..val])
                sieve [] = []
                sieve (p:xs) = p : sieve[x|x <- xs, x `mod` p > 0]
                primes = sieve [2 .. val]
                factors = [primeFac x primes | x <- vals]
            in smallCover primes factors

-- create smallest coverset by finding the largest number of times
-- a factor occurs in any given number
-- maximum occurances is the requirement for the smallest coverset
-- repeat for all possible prime factors
smallCover [] _ = []
smallCover (h:xs) list = replicate (maximum (map (count h) list)) h ++ smallCover xs list

-- count elements in set equal to x
count x = length . filter (==x)

primeFac val [p] = if val `mod` p == 0
                   then [p]
                   else []
primeFac val (p:xs) = if val `mod` p == 0
                      then p : primeFac (val `div` p) (p:xs)
                      else primeFac val xs
