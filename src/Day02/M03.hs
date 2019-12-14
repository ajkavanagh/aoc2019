module Day02.M03
    ( main03
    , run
    ) where

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



main03 :: IO ()
main03 = do
    putStrLn $ "Hi, using: " ++ opcodesFile
    block <- TIO.readFile opcodesFile
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
    let badops = [head opcodes] ++ [12,2] ++ drop 3 opcodes
    putStrLn "The solution is:"
    print $ head $ run badops

