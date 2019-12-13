module Day01.M01 (
    main01
    ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

modules = "files/01/modules.txt"


calc :: Integer -> Integer
calc x = x `div` 3 - 2

main01 :: IO ()
main01 = do
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

