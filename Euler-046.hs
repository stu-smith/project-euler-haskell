--
-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2x1^2
-- 15 = 7 + 2x2^2
-- 21 = 3 + 2x3^2
-- 25 = 7 + 2x3^2
-- 27 = 19 + 2x2^2
-- 33 = 31 + 2x1^2
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
--

primes :: [Int]
primes
    = 2 : 3 : filter isPrime [5,7..]

isPrime :: Int -> Bool
isPrime n
    = all (notDivs n) $ takeWhile (\ p -> p * p <= n) primes
  where notDivs n p = n `mod` p /= 0

oddComposites :: [Int]
oddComposites
    = filter (not . isPrime) [9, 11..]

isSumOfPrimeAndTwiceSquare :: Int -> Bool
isSumOfPrimeAndTwiceSquare n
    = any (isSumOfGivenPrimeAndTwiceSquare n) validPrimes
  where validPrimes = takeWhile (< n) primes

isSumOfGivenPrimeAndTwiceSquare :: Int -> Int -> Bool
isSumOfGivenPrimeAndTwiceSquare n prime
    = any (isSumOfGivenPrimeAndTwiceGivenSquare n prime) values
  where values = [1 .. diff]
        diff   = (n - prime) `div` 2

isSumOfGivenPrimeAndTwiceGivenSquare :: Int -> Int -> Int -> Bool
isSumOfGivenPrimeAndTwiceGivenSquare n prime v
    = n == prime + 2 * v * v


main
    = print $ head $ filter (not . isSumOfPrimeAndTwiceSquare) oddComposites
