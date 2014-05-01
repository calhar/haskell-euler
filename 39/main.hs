import Data.List
import Data.Ord

main = print $ maximumBy (comparing snd) $
          zip [2, 4 ..1000] $ map length $ map (pythagTriple) [2, 4 ..1000]

-- as in euler 09
-- we can get pythag triples using this
--
-- although apparently i need to fix some things here
-- since 120 has (24, 32, 40) showing up for some reason
-- I assume it's something wrong with ((val `div` 2) `div` (b * a))
--
-- Whatever, another filter so that sum of the triple == val works too
pythagTriple val = 
                 let factors = factors_naive (val `div` 2)
                     abc_list = [[b - a, a, ((val `div` 2) `div` (b * a))] |
                                 a <- factors, b <- factors, gtxlt2x a b]
                 in nub $ filter (\t -> sum t == val) $ map nmk2abc abc_list

nmk2abc (m:n:k:[]) = sort [k*((n^2) - (m^2)),
                           (k * (2 * m * n)), 
                           k*((n^2) + (m^2))]

-- check that x < y < 2x
gtxlt2x x y = (y > x) && (y < 2*x)

-- naive factorisation
factors_naive n = [i | i <- [1 .. n], n `mod` i == 0]
