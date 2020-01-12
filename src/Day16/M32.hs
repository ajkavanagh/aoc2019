module Day16.M32 where

import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed         as UV

import           Control.Monad               (foldM, forM_)
import           Control.Monad.Primitive     (PrimState)
import           Control.Monad.ST            (ST, runST)


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


-- this does too much work, and gets killed by the OS; too much memory
doTest :: [Int] -> String
doTest ss = let ss' = concat $ replicate 10000 ss
                rs = doPhases ss' 100
                offset = (read $ concatMap show $ take 7 rs) :: Int
             in concatMap show $ take 8 $ drop offset rs

-- Part2: we have to be cleverer in how we calculate the digits.
--
-- The 2nd half of the signal is:
-- 1. The last digit is always the same after a phase
-- 2. The 2nd last digit is the last + 2nd last (i.e. Xn + Xn-1)
-- 3. the 3rd last is X'n-1 + Xn-2 .. all modulo 10
-- 4. This works for all the digits back to the half way points
--
-- Then for the first half we use only half the digits and sum or substract them
-- based on a start position.
--
-- So for:
-- 0: X0 + X4 + X8 ... -(X2 + X6 + X10) = indices 4n and 4n+2
-- 1: (X1 + X2) - (x5 + x6) + (x9 + x10) - (x13+x14) ...
--    which is (x1 + x9 + x17 ...) + (x2 + x10 + x20) ... - (x5 + x13 + x21) - (x6 + x14 + x22)
--    which are indices [1+8n,2+8n] and [5+8n,6+8n]
-- 2: (x2+x3+x4) - (x8+x9+x10) + (x14+x15+x16) - (x20+x21+x22)
--    which is (2,14,26...) + (3,15,27,...) + (4,16,28...)
--           - (8,20,32..)  - (9,21,33,...) - (10,22,34...)
--    which are indices [2+12n,3+12n,4+12n] and [8+12n,9+12n,10+12n]
--
-- 0 4 8 12 ...    - 2 6 10 14 ...
-- 1,2 9,10 17,18 ... - 5,6 13,14, 21,22
-- 2,3,4 14,15,16 ... - 8,9,10 20,21,22 32,33,34
-- (no obvious way of simplifying)

-- these are the ones we add for digit n (starting from 0)
pluses :: Int -> [Int]
pluses n = let m = (n+1)*4
            in  [x+m*y |  y <- [0..], x <- [n..n*2]]


-- these are the ones we subtract for digit n (starting from 0)
minuses :: Int -> [Int]
minuses n = let m = (n+1) * 4
                o = m `div` 2
             in [o+x+m*y | y <- [0..], x <- [n..n*2]]


type MVInt s  = UV.MVector (PrimState (ST s)) Int

loadVector :: [Int] -> UV.Vector Int
loadVector = UV.fromList


leftCalcDigitNinST :: MVInt s -> Int -> Int -> ST s ()
leftCalcDigitNinST mv l n = do
    let ps = takeWhile (<l) (pluses n)
        ms = takeWhile (<l) (minuses n)
    plsum <- foldM getter 0 ps
    mssum <- foldM getter 0 ps
    VM.write mv n ((plsum - mssum) `mod` 10)
  where
    getter acc i = do
        v <- mv `VM.read` i
        pure $ acc + v


-- this is FAR too slow for 6,500,000 digits
doLeftPhaseST :: MVInt s -> ST s ()
doLeftPhaseST mv = do
    let l = VM.length mv
    forM_ [0..l `div` 2] (leftCalcDigitNinST mv l)


doRightPhaseST :: MVInt s -> ST s ()
doRightPhaseST mv = do
    let l = VM.length mv
    forM_ [l-2,l-3 .. (l `div`2 +1)] adder
  where
      adder i = do
          r <- VM.read mv (i+1)
          v <- VM.read mv i
          VM.write mv i ((r + v) `mod` 10)


doPhaseST :: MVInt s -> ST s ()
doPhaseST mv = do
    doLeftPhaseST mv
    doRightPhaseST mv


doAPhase :: UV.Vector Int -> UV.Vector Int
doAPhase v = runST $ do
    mv <- UV.thaw v
    doPhaseST mv
    UV.freeze mv


-- it turns out that our offset is near the end, and so we can actually just
-- waste time doing the right only.  We only actually need to do back to the
-- offset which would be even quicker
doRightPhaseOnly :: Int -> UV.Vector Int -> UV.Vector Int
doRightPhaseOnly n v = runST $ do
    mv <- UV.thaw v
    forM_ [1..n] (\_ -> doRightPhaseST mv)
    UV.freeze mv


msgAtOffset :: UV.Vector Int -> Int -> Int -> String
msgAtOffset v o l = concatMap (show.(v UV.!)) [o..o+l-1]


main32 :: IO ()
main32 = do
    putStrLn "Day 16: Part 2: Flawed Frequency Transmission - offset messages"
    signal <- loadSignal
    let digits = loadDigits signal
    -- it only works in reasonable time, because the offset (for me) is in the
    -- 2nd half of the signal.
    let offset = (read $ concatMap show $ take 7 digits) :: Int
    putStrLn $ "The offset for the message is: " ++ show offset
    let digits' = concat $ replicate 10000 digits
        dv = loadVector digits'
    putStrLn $ "signal length: " ++ show (UV.length dv)
    let dv' = doRightPhaseOnly 100 dv
    putStrLn $ "The result is " ++ msgAtOffset dv' offset 8
