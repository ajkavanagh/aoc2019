module Day16.M31 where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

signalFile = "files/16/signal.txt"


loadSignal :: IO T.Text
loadSignal = TIO.readFile signalFile


loadDigits ::  T.Text -> [Int]
loadDigits block = map (read . (: [])) (T.unpack $ T.strip block)

-- the cycle generator

cycles :: Int -> [Int]
cycles n = tail $ cycle $ concatMap (replicate n) [0,1,0,-1]


-- calc a value at position n (where n = 0..end)
calcDigitN :: [Int] -> Int -> Int
calcDigitN ss n =
    let ms = zipWith (*) ss (cycles (n+1))
     in abs (sum ms) `mod` 10

doPhase :: [Int] -> [Int]
doPhase ss = map (calcDigitN ss) [0..length ss -1]


doPhases :: [Int] -> Int -> [Int]
doPhases ss n = iterate doPhase ss !! max 0 n


doTest :: [Int] -> String
doTest digits = concatMap show $ take 8 $ doPhases digits 100


main31 :: IO ()
main31 = do
    putStrLn "Day 16: Part 1: Flawed Frequency Transmission"
    signal <- loadSignal
    let digits = loadDigits signal
    putStrLn $ "The result of the signal, after 100 phases is " ++ doTest digits
