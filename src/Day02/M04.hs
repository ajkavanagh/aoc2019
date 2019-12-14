module Day02.M04
    ( main04
    , findSolution
    ) where

-- 04 program workout the bad opcode

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

opcodesFile = "files/02/opcodes.txt"


go :: Int -> [Int] -> [Int]
go n xs = let op = xs !! n
              x1 = xs !! (n+1)
              x2 = xs !! (n+2)
              out = xs !! (n+3)
              res = case op of
                  1 -> (xs !! x1) + (xs !! x2)
                  2 -> (xs !! x1) * (xs !! x2)
                  _ -> 0
           in if op == 99
                then xs
                else go (n+4) $ take out xs ++ [res] ++ drop (out+1) xs

run :: [Int] -> [Int]
run = go 0

runWith :: [Int] -> (Int, Int) -> [Int]
runWith xs (x,y) = run $ [head xs] ++ [x,y] ++ drop 3 xs

findSolution :: T.Text -> Int -> Int
findSolution block target =
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
        pairs = [(x,y) | x <- [0..99], y <- [0..99]]
        badops = [head opcodes] ++ [12,2] ++ drop 3 opcodes
        results = map (runWith opcodes) pairs
        found = head $ dropWhile ((/= 19690720).head) results
     in 100 * (found !! 1) + (found !! 2)

main04 :: IO ()
main04 = do
    putStrLn $ "Hi, using: " ++ opcodesFile
    block <- TIO.readFile opcodesFile
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
    print "Broken code"
    putStrLn "For solution with 19690720 we need to find the output"
    print $ findSolution block 19690720
    -- find 19690720 by running them in the 1st and second places
    {-let pairs = [(x,y) | x <- [0..99], y <- [0..99]]-}
    {-let badops = [head opcodes] ++ [12,2] ++ drop 3 opcodes-}
    {-print badops-}
    {-print $ run badops-}
    {--- find the solution-}
    {-let results = map (runWith opcodes) pairs-}
    {-let found = dropWhile ((/= 19690720).head) results-}
    {-print $ head found-}

