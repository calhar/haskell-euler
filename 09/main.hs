import Data.List

main = print (product (concat(pythagTriple (toInteger(1000)))))

-- neat trick here
-- so according to Euler, any pythagorean triple can be computed using
-- a = k(n^2 - m^2)
-- b = k(2 * m * n)
-- c = k(n^2 + m^2)
-- where m < n
-- As a result
-- a + b + c = X gives
-- k(2 * n^2 + 2 * m * n) = X
-- k * n * (n + m) = X / 2
-- setting s = n + m, n < s < 2n
-- we know that all 3 must be factors of X / 2
-- iterate through factors for n, find factor s, s.t. n < s < 2n
-- and then get k by (X/2)/(n * s)
-- add triple (a, b, c) to list
--
-- simples
--
-- because
-- a = [1..1000]
-- b = [a..1000]
-- c = 1000 - a - b
-- and checking is boring
pythagTriple val = 
                 let factors = factors_naive (val `div` 2)
                     get_s n vals= filter (`gtxlt2x` n) vals 
                     abc_list = concat (
                                  map (\t -> (
                                    map (\x -> [x `subtract` snd(t),
                                                x,
                                                (val `div` 2) `div` (x * snd(t))])
                                    (fst(t)))) (filter ((/=[]).fst) (zip (
                                      map (`get_s` factors) factors) factors)))
                 in nub (filter (/=[0,0,0]) (map nmk2abc abc_list))

nmk2abc (n:m:k:[]) = [k*((n^2) `subtract` (m^2)),
                      (k * (2 * m * n)), 
                      k*((n^2) + (m^2))]

-- check a < b < c for triple
altbltc (a:b:c:[]) = (a < b) && (b < c)

-- check that x < y < 2x
gtxlt2x x y = (y > x) && (y < 2*x)

-- naive factorisation
factors_naive n = [i | i <- [1 .. n], n `mod` i == 0]
