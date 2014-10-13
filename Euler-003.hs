--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
--

import Data.List


factors :: (Integral a) => a -> [a]
factors x = filter (\ y -> x `mod` y == 0) [1 .. limit]
  where limit = ceiling $ sqrt $ fromIntegral x

isPrime x = isNothing $ find (> 1) (factors x)

primeFactors x = filter isPrime $ factors x

main = print $ last $ primeFactors 600851475143
