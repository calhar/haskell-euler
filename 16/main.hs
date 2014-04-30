main = print $ sum $ digits (2^1000)

-- again pretty easy in haskell since it handles bigint stuff automatically
digits = map (`mod` 10) . reverse . takeWhile (>0) . iterate (`div` 10)
