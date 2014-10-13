--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?
--

import List


factors :: (Integral a) => a -> [a]
factors x = filter (\ y -> x /= y && x `mod` y == 0) [1 .. (ceiling $ sqrt $ fromIntegral x)]

isPrime x = (find (> 1) $ factors x) == Nothing

primes = filter isPrime [2..]

main = putStrLn $ show $ primes !! 10000
