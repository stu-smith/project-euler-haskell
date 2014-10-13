--
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--

import List


factors :: (Integral a) => a -> [a]
factors x = filter (\ y -> x `mod` y == 0) [1..x]

isPrime x = (length $ factors x) == 2

primeFactors x = filter isPrime $ factors x

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible x y = x `mod` y == 0

isDivisibleAll :: (Integral a) => a -> [a] -> Bool
isDivisibleAll x = all (\ y -> isDivisible x y)

range = [1..20]
primes = foldl union [] $ map primeFactors range
rangeMax = product primes

rangeReduced = filter (\ x -> not $ isAnyDivisible x $ rangeBigger x ) range
    where rangeBigger x = filter (> x) range
          isAnyDivisible x ys = any (\ y -> isDivisible y x) ys

main = putStrLn $ show $ head $ filter (\ x -> isDivisibleAll x rangeReduced) [rangeMax, rangeMax + rangeMax ..]
