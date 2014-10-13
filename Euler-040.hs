digits = foldr (++) "" $ map show [1..]

extractedDigits = map ((digits !!) . (flip (-) 1) . (10^)) [1..6]

main = print $ product $ map (\c -> read [c] :: Int) extractedDigits
