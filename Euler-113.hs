--
-- Working from left-to-right if no digit is exceeded by the digit to its left
-- it is called an increasing number; for example, 134468.
--
-- Similarly if no digit is exceeded by the digit to its right it is called a
-- decreasing number; for example, 66420.
--
-- We shall call a positive integer that is neither increasing nor decreasing a
-- "bouncy" number; for example, 155349.
--
-- As n increases, the proportion of bouncy numbers below n increases such that
-- there are only 12951 numbers below one-million that are not bouncy and only
-- 277032 non-bouncy numbers below 10^10.
--
-- How many numbers below a googol (10^100) are not bouncy?
--

import Data.List

main = print $ sum $ map (length . nonBouncy) [1..13]

nonBouncy n = increasing n ++ decreasing n

increasing = digitsOrdered alt
    where alt n = [n..9]

decreasing = digitsOrdered alt
    where alt n = [0..n]

digitsOrdered _ 1 = [1..9]
digitsOrdered r n = do p <- digitsOrdered r (n - 1)
                       let last = p `mod` 10
                           next = p * 10
                       alt <- r last
                       return $ next + alt

