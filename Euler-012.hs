main = print $ head $ filter hasEnoughFactors triangles

hasEnoughFactors n = (length $ factors n) > 500

nthTriangle n = n * (2 + (n-1)) `div` 2

triangles = map nthTriangle [1..]

isDivisibleBy n f = n `rem` f == 0

factors :: Int -> [Int]
factors n = (filter (isDivisibleBy n) [1 .. intSqrt n]) >>= (\x -> [x, n `div` x])

intSqrt :: Int -> Int
intSqrt n = truncate $ sqrt $ fromIntegral n
