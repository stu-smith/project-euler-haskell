--
-- n! means n x (n - 1) x ... x 3 x 2 x 1
-- Find the sum of the digits in the number 100!
--

main = putStrLn $ show $ sum $ map (\ x -> read [x] :: Int) $ show (product [1..100])
