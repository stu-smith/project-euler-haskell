import Control.Monad

main
    = do allLines <- getLines "Euler-067.txt"
         let splitLines = map words allLines
         let triangle = map (\ws -> map (\w -> read w ::Int) ws) splitLines
         let maxTriangle = maxPath triangle
         print maxTriangle
  where getLines = liftM lines . readFile

maxPath :: [[Int]] -> [Int]
maxPath (xs:[])
    = xs
maxPath (aboves:belows:rest)
    = zipWith maxOfTwo aboves optionsBelows
  where maxBelows = maxPath (belows:rest)
        optionsBelows = zip maxBelows (tail maxBelows)

maxOfTwo :: Int -> (Int,Int) -> Int
maxOfTwo n (x,y)
    = max (n + x) (n + y)

