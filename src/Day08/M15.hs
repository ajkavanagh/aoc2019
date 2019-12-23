module Day08.M15 where


import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

import           Data.List       (minimumBy)
import qualified Data.List       as L
import           Data.List.Split (chunksOf)

import           Data.Array      (Array, bounds, elems, listArray, (!))


pixelsFile = "files/08/pixels.txt"


type Layer = Array (Int, Int) Int
type Layers = Array Int (Array (Int,Int) Int)

data RawImage = RawImage { sizeW  :: Int
                         , sizeH  :: Int
                         , layers :: Layers
                         }


loadDigits ::  IO [Int]
loadDigits = do
    block <- TIO.readFile pixelsFile
    pure $ map (read . (: [])) (T.unpack $ T.strip block)


-- check the whole set of blocks matches a number of layers for the x/y count
checkBounds :: Int -> Int -> [Int] -> Bool
checkBounds x y l = let block = x * y
                    in length l `mod` block == 0


loadImage :: Int -> Int -> [Int] -> RawImage
loadImage w h digits =
    let size = w * h
        n = length digits `div` size
        blocks = chunksOf size digits
        arrays = map (listArray ((0,0),  (w-1, h-1))) blocks :: [Array (Int,Int) Int]
        layers = listArray (0, n-1) arrays
    in RawImage { sizeW=w, sizeH=h, layers=layers }


countInLayer :: Int -> Layer -> Int
countInLayer v layer = length $ filter (==v) $ elems layer

-- count the value in the layers and return a list of those count against layer
-- numbers
countInLayers :: RawImage -> Int -> [(Int, Int)]
countInLayers img v =
    let ls = layers img
        (s,e) = bounds ls
        ps = zip [s..e] (map (countInLayer v) (elems ls))
    in ps

-- find the minimum layer by looking at the send part in each pair
minLayer :: [(Int,Int)] -> Int
minLayer ps = fst $ minimumBy (\x y -> snd x `compare` snd y) ps

nInLayer :: RawImage -> Int -> Int -> Int
nInLayer img i v = countInLayer v (layers img ! i)


-- finally let's check the solution
verifyImg :: RawImage -> Int
verifyImg img =
    let zeroLayer = minLayer $ countInLayers img 0
        ones = nInLayer img zeroLayer 1
        twos = nInLayer img zeroLayer 2
    in ones * twos


main15 :: IO ()
main15 = do
    putStrLn "AOC2019 Day 8 Part 1 - Space Image Format"
    digits <- loadDigits
    let img = loadImage 25 6 digits
        res = verifyImg img
    putStrLn $ "Checksum is " ++ show res
