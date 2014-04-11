main = print (largestPalindrome 3)

-- get the maximum value of the tuple (x * y, x, y)
-- from the flattened list of the powerset of 3 digit numbers
-- where the digits of x * y are a palindrome
largestPalindrome digNum= do
                            maximum [(round(x * y), x, y) 
                                      | (x, y) <- concat (powerset numbers),
                                      isPalindrome (digits (round(x * y)))]
    where numbers = reverse [10 ** (digNum - 1) .. (10 ** digNum - 1)]

-- get set of every element combined with every other element
-- ignore duplicates
powerset [] = []
powerset nums@(h:xs) = zip (replicate (length nums) h) nums : powerset xs

-- function to get all the digits in order in a list
digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)

-- simple palindrome check
isPalindrome w = w == reverse w
