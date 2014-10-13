--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 x 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
--

import Control.Applicative

isPalindrome :: Integer -> Bool
isPalindrome x = (show x) == (reverse . show) x

threeDigits = [999, 998 .. 100]

main = print $ maximum $ filter isPalindrome ( (*) <$> threeDigits <*> threeDigits )
