import Data.Array

main = print $ sum $ map (\n -> count arr n) [1..100]
    where arr = factorials 100

count arr n = length $ filter isMatch $ map (c arr n) [1..n]

isMatch v = v > 1000000

c arr n r = (arr ! n) `div` ((arr ! r) * (arr ! (n-r)))

factorials max = listArray (0, max) $ map (\x -> product [1..x]) [0..max]
