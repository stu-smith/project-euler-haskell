--
-- A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
--
-- For example,
--
-- 44 -> 32 -> 13 -> 10 -> 1 -> 1
-- 85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89
--
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
-- What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
--
-- How many starting numbers below ten million will arrive at 89?
--

digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits d ++ [m]
  where (d, m) = n `divMod` 10

chainEndsIn89 :: Int -> Bool
chainEndsIn89 n
    | n == 1    = False
    | n == 89   = True
    | otherwise = chainEndsIn89 sumOfSquaresOfDigits
  where sumOfSquaresOfDigits = sum $ map (^2) $ digits n

main
    = print $ length $ filter chainEndsIn89 [1..9999999]
