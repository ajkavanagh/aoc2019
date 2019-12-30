{-# LANGUAGE OverloadedStrings #-}

module Day10.M20 where


import qualified Data.List    as L
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           Data.Vector  (Vector)
import qualified Data.Vector  as V


type Axy = (Int,Int)   -- absolute XY
type Rxy = (Int,Int)   -- relative XY


mapFile :: String
mapFile = "files/10/asteriods.txt"


loadMap :: T.Text -> Vector (Vector Bool)
loadMap = V.fromList . map (stringToVectorBool . T.unpack) . T.lines

charToBool :: Char -> Bool
charToBool '.' = False
charToBool _   = True

stringToVectorBool :: String -> Vector Bool
stringToVectorBool = V.fromList . map charToBool


asteroidCoords :: Vector (Vector Bool) -> Vector Axy
asteroidCoords m = let h = V.length m
                       w = V.length (m V.! 0)
                    in V.fromList [(w',h')
                                  | h' <- [0..h-1]
                                  , w' <- [0..w-1]
                                  , (m V.! h') V.! w']


-- these are asteriods relative to an x,y coord
-- one (if it's on an asteriod) will be (0,0)
relativeAsteroidsFrom :: Vector Axy -> Axy -> Vector Rxy
relativeAsteroidsFrom as (x,y) = V.map (\(x', y') -> (x'-x, y'-y)) as


-- two asteriods are in a line, if they are the same multiples of x and y.
-- Filter any asteroids that are a multiple of each other.

isMultiple :: Rxy -> Rxy -> Bool
isMultiple (0,0) _ = False
isMultiple _ (0,0) = False
isMultiple (x1,y1) (x2,y2) =
    let g1 = gcd x1 y1
        g2 = gcd x2 y2
        (x1',y1') = (x1 `quot` g1, y1 `quot` g1)
        (x2',y2') = (x2 `quot` g2, y2 `quot` g2)
     in x1' == x2' && y1' == y2'


-- we just need 1 asteroid per sight line; it doesn't matter which, but we count
-- them for each location.
filterOut :: Rxy -> Vector Rxy -> Vector Rxy
filterOut xy = V.filter (not . isMultiple xy)

filter00 :: Vector Rxy -> Vector Rxy
filter00 = V.filter (/=(0,0))


visibleFor' :: Vector Rxy -> Vector Rxy -> Vector Rxy
visibleFor' as bs
    | V.null bs = as
    | otherwise = let b = V.head bs
                      bs' = V.tail bs
                   in visibleFor' (as V.++ V.singleton b) (filterOut b bs')

visibleFor :: Vector Rxy -> Vector Rxy
visibleFor = filter00 . visibleFor' V.empty


tagVisbleFor :: Vector Axy -> Vector (Axy, Int)
tagVisbleFor as = V.map tagIt as
  where
      tagIt :: (Int,Int) -> ((Int,Int), Int)
      tagIt xy = let cs = visibleFor (relativeAsteroidsFrom as xy)
                  in (xy, V.length cs)


pickMaximum :: Vector (Axy, Int) -> (Axy, Int)
pickMaximum taggedAs = L.maximumBy ord $ V.toList taggedAs
  where ord a b = snd a `compare` snd b

-- Now for part 2, we need to:
-- 1. for our choosen position, tag the relative asteriod Xy to absolute Xy
-- 2. Sort the list by angle/length of vector or relative asteriods.
-- 3. group by same angle (isMultiple)
-- 4. Them pick off each 1st element, then 2nd, then 3rd, etc. and concat
-- 5. finally tag the remaining list with 1..,
-- This is the sorted list by angle / rounds / distance.  Then just pick the one
-- that is wanted.

tagByAbsolute :: Vector Axy -> Axy -> Vector (Rxy, Axy)
tagByAbsolute as xy = V.zip (relativeAsteroidsFrom as xy) as

filter00tagged :: Vector (Rxy, Axy) -> Vector (Rxy, Axy)
filter00tagged = V.filter ((/=(0,0)).fst)


-- Compare angles of vectors, where (0,-ve) is UP, (0,+ve) is DOWN, (+ve, 0) is
-- RIGHT, (-ve,0) is LEFT, and then angles sweep right.
-- So:
--   /|
--  / |
-- /  |
-- ----
--
-- is a triangle made by the vector (4,-3).  The quadrants go clockwise from
-- 12 o'clock and then we have to say which two vectors are less than or equal.
-- We compare the quadrants first (and if they differ, then it's fairly easy).
--
-- Quadrants are  30
--                21
--
-- going clockwise. Then we'll look at the angles within the quadrants, and
-- finall the size of the vector if the angles are the same.
compareVector :: Rxy -> Rxy -> Ordering
compareVector a@(ax,ay) b@(bx,by) =
    let qa = whichQuadrant a
        qb = whichQuadrant b
        r1 = qa `compare` qb
     in if r1 /= EQ
          then r1
          else let r2 = case qa of
                    0 -> compareFreeAngle (ax,-ay) (bx,-by)
                    1 -> compareFreeAngle a b
                    2 -> compareFreeAngle (-ax,-ay) (-bx,-by)
                    3 -> compareFreeAngle (-ax,ay) (-bx,by)
                in if r2 == EQ
                     then (abs ax + abs ay) `compare` (abs bx + abs by)
                     else r2


whichQuadrant :: Rxy -> Int
whichQuadrant (0,0) = 0
whichQuadrant (0,y) | y < 0 = 0
whichQuadrant (0,y) | y > 0 = 2
whichQuadrant (x,0) | x > 0 = 1
whichQuadrant (x,0) | x < 0 = 3
whichQuadrant (x,y)
    | x > 0 && y < 0 = 0
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | otherwise = error "Programming error with whichQuadrant?"


-- compare the positive vector (+ve, +ve) as though it were on a graph, where x
-- goes to the right, y goes up for (x,y) and steeper is < shallow.
compareFreeAngle :: Rxy -> Rxy -> Ordering
compareFreeAngle (0,ay) (0,by) = ay `compare` by
compareFreeAngle (0,_) _ = LT
compareFreeAngle _ (0,_) = GT
compareFreeAngle (ax,ay) (bx,by) = let ay' = ay * bx
                                       by' = by * ax
                                    in by' `compare` ay'



-- sort the vector to a list that uses the angle and then length (closer)
sortByVector :: Vector (Rxy,Axy) -> [(Rxy,Axy)]
sortByVector = L.sortBy sorter . V.toList
  where
      sorter :: (Rxy,Axy) -> (Rxy,Axy) -> Ordering
      sorter (ra,_) (rb,_) = compareVector ra rb


-- group by the angle (i.e. when the angle is the same
groupByAngle :: [(Rxy,Axy)] -> [[(Rxy,Axy)]]
groupByAngle = L.groupBy grouper
  where
      grouper :: (Rxy,Axy) -> (Rxy,Axy) -> Bool
      grouper (ra,_) (rb,_) = isMultiple ra rb


-- now take the groups and concatenate by taking the first element of each
-- sublist (from groupByAngle) and then the 2nd, etc.  This is the rotating
-- canon which hits the first item each time.
scanRotate :: [[a]] -> [a]
scanRotate = scanRotate' [[]]


-- this does the work with take1drop1 and extractSweep.
scanRotate' :: [[a]] -> [[a]] -> [a]
scanRotate' ax bbx =
    let (ax',bbx') = extractSweep bbx
     in if null ax'
          then concat $ reverse ax
          else scanRotate' (ax':ax) bbx'


take1drop1 :: [[a]] -> [([a], [a])]
take1drop1 = map taker
  where
      taker :: [a] -> ([a], [a])
      taker ax = (take 1 ax, drop 1 ax)


extractSweep :: [[a]] -> ([a], [[a]])
extractSweep as = (concat $ fst as', snd as')
  where as' = unzip $ take1drop1 as


-- some tests
testMap5 :: T.Text
testMap5 = T.unlines [ ".#..##.###...#######"
                     , "##.############..##."
                     , ".#.######.########.#"
                     , ".###.#######.####.#."
                     , "#####.##.#.##.###.##"
                     , "..#####..#.#########"
                     , "####################"
                     , "#.####....###.#.#.##"
                     , "##.#################"
                     , "#####.##.###..####.."
                     , "..######..##.#######"
                     , "####.##.####...##..#"
                     , ".#####..#.######.###"
                     , "##...#.##########..."
                     , "#.##########.#######"
                     , ".####.#.###.###.#.##"
                     , "....##.##.###..#####"
                     , ".#.#.###########.###"
                     , "#.#.#.#####.####.###"
                     , "###.##.####.##.#..##"
                     ]

{-solveForTestMap5 :: Vector (Rxy,Axy)-}
solveForTestMap5 =
    let as = asteroidCoords $ loadMap testMap5
        loc = (11,13)
        rs = filter00tagged $ tagByAbsolute as loc
        ss = sortByVector rs
        os = groupByAngle ss
        fv = V.fromList $ scanRotate os
    in fv


-- now for the solution
main20 :: IO ()
main20 = do
    putStrLn "Day 10: Part 2: Monitoring Station"
    mapText <- TIO.readFile mapFile
    let as = asteroidCoords $ loadMap mapText
        loc = pickMaximum $ tagVisbleFor as
    putStrLn $ "Result location was: " ++ show loc
    let axy = fst loc
        rs = filter00tagged $ tagByAbsolute as axy
        os = groupByAngle $ sortByVector rs
        -- and complete the ordered result
        fv = V.fromList $ scanRotate os
    putStrLn $ "The two hundredth item is " ++ show (fv V.! 199)



