import Data.Array.Unboxed


magic = 28123

divisors n = filter (\x -> n `rem` x == 0) [1 .. n-1]

isAbundant n = sum (divisors n) > n

abundants :: UArray Int Int
abundants = listArray (0, length ns - 1) ns
  where ns = filter isAbundant [12..magic]

exists array value = chop array value alo ahi
  where (alo,ahi) = bounds array
        chop array value lo hi
            | hi < lo = False
            | pivot > value = chop array value lo (mid-1)
            | pivot < value = chop array value (mid+1) hi
            | otherwise = True
          where mid   = lo + (hi-lo) `div` 2
                pivot = array ! mid

isSumOfTwoAbundants n = iterate 0
  where iterate index
            | nthAbundant >= n                   = False
            | exists abundants (n - nthAbundant) = True
            | otherwise                          = iterate (index + 1)
          where nthAbundant = abundants ! index

matches = filter (not . isSumOfTwoAbundants) [1..magic]

main = print (sum matches)
