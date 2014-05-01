import Data.List.Utils
import Data.List
import Data.Ord

-- so we have two large exponentials of the form a^b and x^y
--
-- obviously taking the logarithm of these in base a or x
-- will allow us to easily compare them
--
-- because
-- if a^b = x^y
-- log (a^b) = log (x^y)
-- => b * log(a) = y * log(x)
--
--
-- don't know why, but I felt like taking them as log_a, but whatever
-- log_a (a^b) = b
-- log_a (x ^ y) = y * (log_a (a * x / a))
--               = y * (log_a (a) + log_a (x / a))
--               = y + y * log_a (ratio)
-- 
-- read lines from file and convert each val a^b to a tuple (a, b)
-- zip with the infinite list to get into the form (index, (a, b))
--
-- then compare (i, (a, b)) (i2, (x, y)) by comparing b with y * log_a (x)
--
main = do
        contents <- readFile "base_exp.txt"
        print $ maximumBy (tupCompare snd) $ zip [1..] $ map toTuple $ lines contents

toTuple str = tuplify2 $ map (\t -> read t :: Double) $ split "," str

tuplify2 [x, y] = (x, y)

tupCompare p x y = expCompare (p x) (p y)
expCompare (v, w) (x, y) = compare (w * (log v)) (y * (log x))
