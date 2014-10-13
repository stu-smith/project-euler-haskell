--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--

import Control.Applicative

main
    = print $ product $ head triples

triples
    = filter (\ (a:b:c:xs) -> predicate a b c)
    $ (\ b c -> [1000-b-c,b,c]) <$> [1..999] <*> [1..999]
  where predicate a b c = a + b + c == 1000 && a > 0 && a < b && b < c && a^2 + b^2 == c^2

