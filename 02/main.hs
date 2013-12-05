main = print (evenFib 4000000)

evenFib :: (Ord a, Integral a) => a -> a
evenFib limit = sum [ z | z <- fibLim [] limit, z `mod` 2 == 0]

fibLim :: (Ord a, Integral a) => [a] -> a -> [a]
fibLim [] limit = fibLim [1] limit
fibLim [1] limit = fibLim [2, 1] limit
fibLim [x,y] limit = fibLim [x+y,x,y] limit
fibLim numbers@(x:y:_) limit = if x + y < limit then 
                                (fibLim ((x+y):numbers) limit)
                              else
                                numbers

