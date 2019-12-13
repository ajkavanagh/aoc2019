module Day01.M02 (
    main02
    ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

modules = "files/01/modules.txt"


calc :: Integer -> Integer
calc x = x `div` 3 - 2

rcalc :: Integer -> Integer
rcalc x = sum $ tail $ takeWhile (>0) $ iterate calc x

main02 :: IO ()
main02 = do
    putStrLn $ "Hi, using: " ++ modules
    block <- TIO.readFile modules
    let lines = (map (read . T.unpack) $ T.lines block) :: [Integer]
    print lines
    print $ calc 12
    print $ calc 14
    print $ calc 1969
    print $ calc 100756
    let total = sum $ map calc lines
    putStrLn $ "Total for all modules is " ++ show total
    print $ rcalc 14
    print $ rcalc 1969
    print $ rcalc 100756
    let rtotal = sum $ map rcalc lines
    putStrLn $ "Total for all modules include fuel on fuel is " ++ show rtotal

