module Day08.M16 where


import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

import           Data.List       (foldl', minimumBy)
import qualified Data.List       as L
import           Data.List.Split (chunksOf)

import           Data.Array      (Array, bounds, elems, listArray, (!))


pixelsFile = "files/08/pixels.txt"


-- note arrays are loaded (lowest index first) and so we need to specify the
-- height index before the width index for 2 dimensional arrays
type Layer = Array (Int, Int) Int
type Layers = Array Int (Array (Int,Int) Int)

data RawImage = RawImage { sizeW  :: Int
                         , sizeH  :: Int
                         , layers :: Layers
                         }


instance Show RawImage where
    show ri = "RawImage <W:" ++ show (sizeW ri)
           ++ ", H:" ++ show (sizeH ri)
           ++ ", n=" ++ show (snd (bounds (layers ri)))
           ++ ">"


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
        arrays = map (listArray ((0,0),  (h-1, w-1))) blocks :: [Array (Int,Int) Int]
        layers = listArray (0, n-1) arrays
    in RawImage { sizeW=w, sizeH=h, layers=layers }


img :: IO RawImage
img = loadImage 25 6 <$> loadDigits


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

-- and build the actual image from the RawImage
decodeImg :: RawImage -> Layer
decodeImg img = listArray bs ps
  where
      w = sizeW img
      h = sizeH img
      ps = [decodePixel img x y | y <- [0..h-1], x <- [0..w-1]]
      bs = ((0, 0), (h-1, w-1))


decodePixel :: RawImage -> Int -> Int -> Int
decodePixel img x y = foldl' go ((layers img ! s) ! (y,x)) column
  where
      (s,e) = bounds $ layers img
      go :: Int -> Int -> Int
      go last next = if last == 2 then next else last
      column = [(layers img ! l) ! (y,x) | l <- [s+1..e]]


column :: RawImage -> (Int, Int) -> [Int]
column img (x,y)  = [(layers img ! l) ! (y,x) | l <- [s..e]]
  where (s,e) = bounds $ layers img


showLayer :: Layer -> String
showLayer lyr = L.unlines $ chunksOf (w+1) [intToChar p | p <- elems lyr]
  where
      (_ , (_, w)) = bounds lyr
      intToChar :: Int -> Char
      intToChar 0 = ' '
      intToChar 1 = 'W'
      intToChar _ = '.'

showLayerN :: Layer -> String
showLayerN lyr = L.unlines $ chunksOf (w+1) $ concat [show p | p <- elems lyr]
  where
      (_, (_, w)) = bounds lyr

main16 :: IO ()
main16 = do
    putStrLn "AOC2019 Day 8 Part 2 - Space Image Format - the image"
    digits <- loadDigits
    let img = loadImage 25 6 digits
        res = verifyImg img
    putStrLn $ "Checksum is " ++ show res
    putStrLn "And the image is"
    putStrLn $ showLayerN $ decodeImg img
    putStrLn $ showLayer $ decodeImg img

