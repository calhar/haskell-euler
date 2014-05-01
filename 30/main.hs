main = print $ sum $ nthPowers 5 

-- absolute upper bound of 9^6
-- because 9^6 = 9 * (9^5) which would be 999999999
--
-- Real upper bound is something like when
-- 10^n > n * 9^5
-- but 9^6 will do
--
pow5s = [n | n <- [2.. 9^6], n == sum (map (^5) (digits n))]

-- Now it's generalised
nthPowers x = [n | n <- [2.. 9^(x + 1)], n == sum (map (^x) (digits n))]

digits = map (`mod` 10) . reverse . takeWhile (>0) . iterate (`div` 10)
