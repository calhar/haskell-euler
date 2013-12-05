main = print ( multiples 1000 3 5 )

multiples :: (Integral a) => a -> a -> a -> a
multiples limit x y = sum [ z | z <- [1..1000], z `mod` x == 0 || z `mod` y == 0]

