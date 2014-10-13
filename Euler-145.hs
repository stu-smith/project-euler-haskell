--
-- Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits.
-- For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible.
-- Leading zeroes are not allowed in either n or reverse(n).
--
-- There are 120 reversible numbers below one-thousand.
--
-- How many reversible numbers are there below one-billion (10^9)?
--

-- ghc -O3 -threaded Euler-145.hs && Euler-145.exe +RTS -N4

{-# LANGUAGE BangPatterns #-}

import Control.Parallel.Strategies

foldDigits :: (a -> Int -> a) -> a -> Int -> a
foldDigits f
    = go
  where go !acc 0 = acc
        go acc n = case n `quotRem` 10 of
                     (q,r) -> go (f acc r) q

reverseNumber :: Int -> Int
reverseNumber !n
    = foldDigits accumulate 0 n
  where accumulate !v !d = v * 10 + d

allDigitsOdd :: Int -> Bool
allDigitsOdd n
    = foldDigits andOdd True n
  where andOdd !a d = a && isOdd d
        isOdd !x    = x `rem` 2 /= 0

isReversible :: Int -> Bool
isReversible n
    = notDivisibleByTen n && allDigitsOdd (n + rn)
  where rn                   = reverseNumber n
        notDivisibleByTen !x = x `rem` 10 /= 0

countRange acc start end
    | start > end = acc
    | otherwise   = countRange (acc + v) (start + 1) end
  where v = if isReversible start then 1 else 0

main
    = print $ sum $ parMap rseq cr ranges
  where max       = 1000000000
        qmax      = max `div` 4
        ranges    = [(1, qmax), (qmax, qmax * 2), (qmax * 2, qmax * 3), (qmax * 3, max)]
        cr (s, e) = countRange 0 s e
