import Debug.Trace

main = print(primeFac 600851475143 [])

primeFac 1 _ = [1]
primeFac val [] = primeFac val (primes val)
primeFac val [p] = if val `mod` p == 0
                   then [p]
                   else []
primeFac val (p:xs) = if val `mod` p == 0
                      then p : primeFac (val `div` p) (p:xs)
                      else primeFac val xs

-- super simple sieve of eratothenes
-- kinda slow really
primes :: (Integral a) => a -> [a]
primes max = sieve [2..max]
  where sieve [] = []
        sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
