{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day17.M33
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.List         (break, foldl', intercalate, maximumBy,
                                    minimumBy, permutations, reverse)

import Data.Char (chr)

import           Control.Monad     (void)
import           Safe              (headMay)

import           Lens.Micro        (each, ix, over, (%~), (&), (.~), (?~), (^.),
                                    (^..))
import           Lens.Micro.TH     (makeLenses)
import           Linear.V2         (V2 (..), _x, _y)

import qualified Data.HashMap.Lazy as H


-- OpCode Machine
import           OpCodeMachine     (Machine, MachineException, loadMachine,
                                    loadOpcodes, runMachineWithInput)

-- Drawing
import           Drawing           (Coord, drawWith, getDimens)

--
-- now the test parts
opcodesFile = "files/17/opcodes.txt"


runCamera :: [Int] -> [Int]
runCamera opcodes =
    let (m, os) = runMachineWithInput [] (loadMachine opcodes)
     in os


showImage :: [Int] -> String
showImage = map chr

-- load the image into a Image

type Image = H.HashMap Coord Int

-- this only is useful for the intersections.
decodeImage :: [Int] -> Image
decodeImage os = H.fromList $ fst $ foldl' go ([], V2 0 0) os
  where
      go :: ([(Coord, Int)], Coord) -> Int -> ([(Coord, Int)], Coord)
      go (xs, xy@(V2 x y)) i
        | i == 10 = (xs, V2 0 (y+1))
        | i == 35 = ((xy, i):xs, xy + V2 1 0)
        | otherwise = (xs, xy + V2 1 0)


displayImage :: Image -> String
displayImage = drawWith chr 32

-- above, below, right and left
deltas :: [Coord]
deltas = [V2 (-1) 0, V2 1 0, V2 0 1, V2 0 (-1)]


-- find the intersections
itersections :: Image -> [Coord]
itersections im = map fst
                $ filter (isIntersection im)
                $ H.toList im


isIntersection :: Image -> (Coord, Int) -> Bool
isIntersection im (xy, v)
  | v /= 35 = False
  | otherwise = let adjacents = map (\d -> H.lookupDefault 0 (xy + d) im) deltas
                 in length (filter (==35) adjacents) == 4


calcResult :: [Coord] -> Int
calcResult cs = sum $ map (\(V2 x y) -> x * y) cs


main33 :: IO ()
main33 = do
    putStrLn "AOC2019 Day 17 Part 1 - Set and Forget"
    opcodes <- loadOpcodes opcodesFile
    let s = runCamera opcodes
    {-putStrLn $ showImage s-}
    let im = decodeImage s
    putStrLn $ displayImage im
    print (itersections im)
    putStrLn $ "The result is " ++ show (calcResult (itersections im))

