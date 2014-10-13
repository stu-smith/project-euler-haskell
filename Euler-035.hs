--
-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?
--

import List
import Maybe

primes :: [Integer]
primes = 2 : 3 : filter isPrime [5,7..]

isPrime n = all (notDivs n) $ takeWhile (\ p -> p * p <= n) primes
    where notDivs n p = n `mod` p /= 0

rotate :: String -> String
rotate []     = []
rotate (x:xs) = xs ++ [x]

rotations :: Integer -> [Integer]
rotations x = map (\ x -> read x :: Integer) $ rotateRecurse (length $ show x) (show x)
    where
        rotateRecurse 0 x = []
        rotateRecurse n x = union [x] $ rotateRecurse (n-1) (rotate x)

isCircularPrime :: Integer -> Bool
isCircularPrime x = all isPrime $ rotations x

main = putStrLn $ show $ length $ filter isCircularPrime $ takeWhile (<1000000) primes

