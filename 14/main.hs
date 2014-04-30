import Data.List
import Data.Ord
main = print $ maximumBy (comparing snd) $ map (\x -> (x, (collatzLen x))) [500000..999999]

-- slight improved implementation
--
-- for each val [500000..999999] since any val in [1..499999] will have another
-- potential value in [500000..999999]
--
-- get collatz length -- don't bother making list and taking length
--
-- return the maximum given by comparing the second item in the tuple
--
-- a better way to do it would be to store lengths as we go and when we reach a
-- known value add and stop
collatzLen val
  | val == 1 = 1
  | val `mod` 2 == 0 = 1 +  collatzLen (val `div` 2)
  | otherwise = 1 +  collatzLen (3 * val + 1)
