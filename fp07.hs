
pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], x == sum (init (factors x))]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

scalarp :: [Int] -> [Int] -> Int
scalarp xs ys = sum [ x*y | (x,y) <- zip xs ys ]