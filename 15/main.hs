main = print $ lat_paths 20

-- the problem of finding the number of paths through a lattice
-- from (0, 0) to (a, b) when you can only move like (1, 0) or (0, 1)
-- can be found using (a + b) chose a
--
-- More specifically, it can be approximated as having to make (a + b)
-- decisions whether you move (1, 0) or (0, 1)
-- Given this, we can find all the possible combinations
-- by listing all the possible decision points that we can move
-- one direction a times given (a + b) decisions
--
-- ie. Given a 2x2 lattice, we will make 4 decisions
-- We need to move right twice, so we can enumerate all the ways
-- we can move right two times as
-- [1, 2] - right right down down
-- [1, 3] - right down right down
-- [2, 3] - down right right down
-- [1, 4] - right down down right
-- [2, 4] - down right down right
-- [3, 4] - down down right right
--
-- If we wanted to list all the combinations then we would use
-- combinations a [1..(a+b)]
--
-- However, we just want the number of combinations
-- which we know to be the binomial coefficient or (a + b) choose a
lat_paths x = nChooseK (2 * x) x

nChooseK n k = (factorial n) `div` ((factorial k) * factorial (n - k))

factorial n = product [1..n]
