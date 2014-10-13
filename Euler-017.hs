--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
--

english :: Int -> String
english n
    | n < 0  = "minus " ++ english (negate n)
    | n < 20 =
                [ "zero", "one", "two", "three", "four"
                , "five", "six", "seven", "eight", "nine"
                , "ten", "eleven", "twelve", "thirteen", "fourteen"
                , "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
                ] !! n
    | n < 100 =
                [ "?", "?", "twenty", "thirty", "forty"
                , "fifty", "sixty", "seventy", "eighty", "ninety"
                ] !! (n `div` 10)
              ++ (if n `mod` 10 == 0 then "" else "-" ++ english (n `mod` 10))
    | n < 1000 =
                (english (n `div` 100)) ++ " hundred"
              ++ (if n `mod` 100 == 0 then "" else " and " ++ english (n `mod` 100))
    | n < 1000000 =
                (english (n `div` 1000)) ++ " thousand"
              ++ (if n `mod` 1000 == 0 then "" else " " ++ english (n `mod` 1000))

main = putStrLn $ show $ length $ filter (`elem` ['a'..'z']) $ foldr (++) [] $ map english [1..1000]

