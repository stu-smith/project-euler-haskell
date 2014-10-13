--
-- The nth term of the sequence of triangle numbers is given by, tn = (1/2)n(n+1);
-- so the first ten triangle numbers are:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its alphabetical
-- position and adding these values we form a word value. For example, the word value
-- for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we
-- shall call the word a triangle word.
--
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle words?
--

import Data.Char

main
    = do contents <- readFile "words.txt"
         let words = read ("[" ++ contents ++ "]") :: [String]
         print $ length $ filter isTriangle $ map wordValue words

wordValue s
    = sum $ map letterValue s

letterValue c
    = ord c - ord 'A' + 1

triangle n
    = n * (n+1) `div` 2

isTriangle v
    = v `elem` candidates
  where candidates = takeWhile (<= v) $ map triangle [1..]
