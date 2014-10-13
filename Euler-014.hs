--
-- The following iterative sequence is defined for the set of positive integers:
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
--

import Data.Ord
import qualified Data.Foldable as Foldable
import Data.Maybe
import qualified Data.Map as Map

searchTo = 1000000

nextNumber :: Integer -> Integer
nextNumber n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

iterateSequence :: Map.Map Integer Int -> Integer -> Map.Map Integer Int
iterateSequence map n
    | Map.member n map = map
    | otherwise        = Map.insert n (1 + nextLength) biggerMap
        where next       = nextNumber n
              biggerMap  = iterateSequence map next
              nextLength = fromJust $ Map.lookup next biggerMap

allSequences = foldl iterateSequence (Map.fromList [(1,1)]) [1,3..searchTo]

longestSequence = Foldable.maximumBy (comparing snd) $ Map.toList allSequences

main = putStrLn $ show longestSequence

