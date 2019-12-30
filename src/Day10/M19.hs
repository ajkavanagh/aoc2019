{-# LANGUAGE OverloadedStrings #-}

module Day10.M19 where


import qualified Data.List    as L
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           Data.Vector  (Vector)
import qualified Data.Vector  as V


mapFile :: String
mapFile = "files/10/asteriods.txt"


loadMap :: T.Text -> Vector (Vector Bool)
loadMap = V.fromList . map (stringToVectorBool . T.unpack) . T.lines

charToBool :: Char -> Bool
charToBool '.' = False
charToBool _   = True

stringToVectorBool :: String -> Vector Bool
stringToVectorBool = V.fromList . map charToBool


asteroidCoords :: Vector (Vector Bool) -> Vector (Int, Int)
asteroidCoords m = let h = V.length m
                       w = V.length (m V.! 0)
                    in V.fromList [(w',h')
                                  | h' <- [0..h-1]
                                  , w' <- [0..w-1]
                                  , (m V.! h') V.! w']


-- these are asteriods relative to an x,y coord
-- one (if it's on an asteriod) will be (0,0)
relativeAsteroidsFrom :: Vector (Int, Int) -> (Int, Int) -> Vector (Int, Int)
relativeAsteroidsFrom as (x,y) = V.map (\(x', y') -> (x'-x, y'-y)) as


-- two asteriods are in a line, if they are the same multiples of x and y.
-- Filter any asteroids that are a multiple of each other.

isMultiple :: (Int,Int) -> (Int,Int) -> Bool
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
filterOut :: (Int,Int) -> Vector (Int,Int) -> Vector (Int,Int)
filterOut xy = V.filter (not . isMultiple xy)

filter00 :: Vector (Int,Int) -> Vector (Int,Int)
filter00 = V.filter (/=(0,0))


visibleFor' :: Vector (Int,Int) -> Vector (Int,Int) -> Vector (Int,Int)
visibleFor' as bs
    | V.null bs = as
    | otherwise = let b = V.head bs
                      bs' = V.tail bs
                   in visibleFor' (as V.++ V.singleton b) (filterOut b bs')

visibleFor :: Vector (Int,Int) -> Vector (Int,Int)
visibleFor = filter00 . visibleFor' V.empty


tagVisbleFor :: Vector (Int,Int) -> Vector ((Int,Int), Int)
tagVisbleFor as = V.map tagIt as
  where
      tagIt :: (Int,Int) -> ((Int,Int), Int)
      tagIt xy = let cs = visibleFor (relativeAsteroidsFrom as xy)
                  in (xy, V.length cs)


pickMaximum :: Vector ((Int,Int), Int) -> ((Int,Int), Int)
pickMaximum taggedAs = L.maximumBy ord $ V.toList taggedAs
  where ord a b = snd a `compare` snd b


-- some tests
testMap1 :: T.Text
testMap1 = T.unlines [ ".#..#"
                     , "....."
                     , "#####"
                     , "....#"
                     , "...##"
                     ]

testMap2 :: T.Text
testMap2 = T.unlines [ "......#.#."
                     , "#..#.#...."
                     , "..#######."
                     , ".#.#.###.."
                     , ".#..#....."
                     , "..#....#.#"
                     , "#..#....#."
                     , ".##.#..###"
                     , "##...#..#."
                     , ".#....####"
                     ]

testMap3 :: T.Text
testMap3 = T.unlines [ "#.#...#.#."
                     , ".###....#."
                     , ".#....#..."
                     , "##.#.#.#.#"
                     , "....#.#.#."
                     , ".##..###.#"
                     , "..#...##.."
                     , "..##....##"
                     , "......#..."
                     , ".####.###."
                     ]

testMap4 :: T.Text
testMap4 = T.unlines [ ".#..#..###"
                     , "####.###.#"
                     , "....###.#."
                     , "..###.##.#"
                     , "##.##.#.#."
                     , "....###..#"
                     , "..#.#..#.#"
                     , "#..#.#.###"
                     , ".##...##.#"
                     , ".....#.#.."
                     ]

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

-- now for the solution

main19 :: IO ()
main19 = do
    putStrLn "Day 10: Part 1: Monitoring Station"
    mapText <- TIO.readFile mapFile
    let as = asteroidCoords $ loadMap mapText
        loc = pickMaximum $ tagVisbleFor as
    putStrLn $ "Result location was: " ++ show loc
