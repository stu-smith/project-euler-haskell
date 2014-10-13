--
-- Take the number 192 and multiply it by each of 1, 2, and 3:
--
-- 192 x 1 = 192
-- 192 x 2 = 384
-- 192 x 3 = 576
--
-- By concatenating each product we get the 1 to 9 pandigital, 192384576.
-- We will call 192384576 the concatenated product of 192 and (1,2,3)
--
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital,
-- 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
--
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an
-- Integereger with (1,2, ... , n) where n  1?
--

import Data.List

concatValues :: [Integer] -> Integer
concatValues
    = read . foldl (++) [] . map show

concatValue :: Integer -> Integer -> Integer
concatValue n v
    = concatValues $ map (v *) [1..n]

isPanDigital :: Integer -> Bool
isPanDigital n
    | n < 123456789 || n > 987654321 = False
    | otherwise                      = sort (show n) == "123456789"

candidatesForN :: Integer -> [Integer]
candidatesForN n
    = map (concatValue n) [1..9999]

allPanDigitals :: [Integer]
allPanDigitals
    = filter isPanDigital $ concat $ map candidatesForN [1..9]

main
    = print $ head $ sortBy (flip compare) allPanDigitals