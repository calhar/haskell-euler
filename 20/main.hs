import Data.Char

-- once again saved by Haskell native support for bigint
-- fac function is pretty simple, infact product [] = 1
-- so this basically works perfectly
--
main = print $ sum $ map digitToInt $ show (fac 100)

fac n = product [n, n-1..1]
