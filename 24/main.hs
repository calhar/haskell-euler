import Data.List

-- super simple
-- so get all possible permutations of [0,1,2,3,4,5,6,7,8,9]
-- sort them
-- take the 1000000th
-- 
-- faster way would probably be generate permutations in order
-- and keep track as you go
-- rather than generating them and then sorting
main = print $ (sort $ permutations "0123456789") !! 999999


