module Day02.M04
    ( main04
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

main04 :: IO ()
main04 = do
    putStrLn $ "Hi, using: " ++ opcodesFile
    block <- TIO.readFile opcodesFile
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
    print opcodes
    print $ go 0 [1,0,0,0,99]
    print $ go 0 [2,3,0,3,99]
    print $ go 0 [2,4,4,5,99,0]
    print $ run [1,1,1,4,99,5,6,0,99]
    print $ run opcodes
    print "Broken code"
    -- find 19690720 by running them in the 1st and second places
    let pairs = [(x,y) | x <- [0..99], y <- [0..99]]
    let badops = [head opcodes] ++ [12,2] ++ drop 3 opcodes
    print badops
    print $ run badops
    -- find the solution
    let results = map (runWith opcodes) pairs
    let found = dropWhile ((/= 19690720).head) results
    print $ head found

