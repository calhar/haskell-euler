main = print $ sum (primes 2000000)

-- super simple sieve of eratothenes
-- kinda slow really
primes :: (Integral a) => a -> [a]
primes max = sieve (2:3:[5, 7 ..max])
  where sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
