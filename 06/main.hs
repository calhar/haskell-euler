main = print (squareDiff 10)

-- quite simple, for first val natural numbers
-- sum vals and take square
-- and take square of each individual val and sum total
squareDiff val =
               let vals = [1 .. val]
               in ((sum vals) ** 2) - sum (map (**2) vals)
