--
-- Euler published the remarkable quadratic formula:
--
-- n^2 + n + 41
--
-- It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
-- However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when
-- n = 41, 41² + 41 + 41 is clearly divisible by 41.
--
-- Using computers, the incredible formula n^2  79n + 1601 was discovered, which produces
-- 80 primes for the consecutive values n = 0 to 79. The product of the coefficients,
-- 79 and 1601, is 126479.
--
-- Considering quadratics of the form:
--
-- n^2 + an + b, where |a| < 1000 and |b| < 1000
--
-- where |n| is the modulus/absolute value of n
-- e.g. |11| = 11 and |4| = 4
-- Find the product of the coefficients, a and b, for the quadratic expression that produces
-- the maximum number of primes for consecutive values of n, starting with n = 0.
--

import List
import Maybe
import Data.Ord

primes
    = 2 : 3 : filter isPrime [5,7..]

isPrime n
    = all (notDivs n) $ takeWhile (\ p -> p * p <= n) primes
  where notDivs n p = n `mod` p /= 0

numPrimesInQuadratic a b
    = length $ takeWhile nthIsPrime [0..]
  where nthIsPrime n = isPrime $ toInteger $ abs $ n * n + a * n + b

consecutives
    = do a <- [-999 .. 999]
         b <- [-999 .. 999]
         return (a * b, numPrimesInQuadratic a b)

main
    = print $ maximumBy (comparing snd) consecutives
