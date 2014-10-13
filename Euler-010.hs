--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
--

import List
import Maybe

primes :: [Integer]
primes = 2 : 3 : filter isPrime [5,7..]
    where
        isPrime n   = all (notDivs n) $ takeWhile (\ p -> p * p <= n) (tail primes)
        notDivs n p = n `mod` p /= 0

main = putStrLn $ show $ sum $ takeWhile (< 2000000) primes
