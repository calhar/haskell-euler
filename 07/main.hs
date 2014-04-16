main = print(last (take 10001 primes))

-- again using a pretty simple sieve of eratothenes
-- generate primes and take the 10001st prime
primes = sieve [2..]
  where sieve [] = []
        sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
