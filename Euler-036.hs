--
-- The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include leading zeros.
--

{-# LANGUAGE NoMonomorphismRestriction #-}

isPalindrome f s = f s == (reverse $ f s)
isPalindrome10   = isPalindrome show
isPalindrome2    = isPalindrome showBinary

showBinary 0 = ""
showBinary n = showBinary (n `div` 2) ++ show (n `mod` 2)

main = putStrLn $ show $ sum $
         filter (\ x -> isPalindrome10 x && isPalindrome2 x) [1..999999]

