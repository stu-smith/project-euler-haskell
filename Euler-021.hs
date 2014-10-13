{-# LANGUAGE BangPatterns #-}

import Data.Array.Unboxed

main = putStrLn $ show sumAmicables

sumAmicables = sum $ amicables 9999

amicables max = filter (amicable arr max) [1..max]
   where !arr = sumDivisors max

amicable :: UArray Int Int -> Int -> Int -> Bool
amicable arr max n = any (amicablePair arr n) [1 .. n-1] || any (amicablePair arr n) [n+1 .. max]

amicablePair :: UArray Int Int -> Int -> Int -> Bool
amicablePair arr !a !b = ((arr ! a) == b) && ((arr ! b) == a)

sumDivisors :: Int -> UArray Int Int
sumDivisors max = listArray (1, max) $ map d [1..max]

d n = sum $ divisors n

divisors n = filter (\x -> n `rem` x == 0) [1 .. n-1]
