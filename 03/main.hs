import Debug.Trace

main = print(primeFac 600851475143 [])

primeFac 1 _ = [1]
-- if we have no primes, highest possible prime is val
-- should really do something to check if sqrt(val) < p
-- so val is only remaining factor and prime
primeFac val [] = primeFac val (primes val)
-- if only element in list
primeFac val [p] = if val `mod` p == 0
                   then [p]
                   else []
-- check if p is factor of val
-- if so div val p and check again
-- else discard p
primeFac val (p:xs) = if val `mod` p == 0
                      then p : primeFac (val `div` p) (p:xs)
                      else primeFac val xs

-- super simple sieve of eratothenes
-- kinda slow really
primes :: (Integral a) => a -> [a]
primes max = sieve [2..max]
  where sieve [] = []
        sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
