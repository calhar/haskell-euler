main = print $ maximum $ zip (map length $ map collatz [1..999999]) [1..999999]

-- simple implementation
--
-- for each val [1..999999]
-- get collatz sequence and take length
--
-- zip with [1..999999] to get collatz length and val in form (length, val)
-- take maximum
--
-- a better way to do it would be to store created lists as we go in linked
-- lists, but whatever
collatz val
  | val == 1 = [1]
  | val `mod` 2 == 0 = val : collatz (val `div` 2)
  | otherwise = val : collatz (3 * val + 1)
