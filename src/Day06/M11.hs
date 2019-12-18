{-# LANGUAGE OverloadedStrings    #-}
module Day06.M11 where



import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import qualified Data.List as L
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Lazy as H
import qualified Data.HashSet as HS

orbitsFile = "files/06/orbits.txt"


decodeOrbitsBlock :: T.Text -> H.HashMap T.Text T.Text
decodeOrbitsBlock txt =
    let ts = T.lines txt
        lx = map (T.split (==')')) ts
        px = map makePair lx
    in H.fromList px
   where makePair :: [T.Text] -> (T.Text, T.Text)
         makePair [x,y] = (y, x)
         makePair _ = error "Problem with list?"


countOrbitsFor :: H.HashMap T.Text T.Text -> T.Text -> Int
countOrbitsFor m k = L.length $ L.unfoldr follow k
  where
      follow :: T.Text -> Maybe (Int, T.Text)
      follow t = H.lookup t m >>= \v -> Just (0,v)


countAllOrbits :: H.HashMap T.Text T.Text -> Int
countAllOrbits m = sum $ map (countOrbitsFor m) $ HS.toList $ H.keysSet m

testSet :: H.HashMap T.Text T.Text
testSet =
    let orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
     in decodeOrbitsBlock orbits


puzzleOrbits :: IO (H.HashMap T.Text T.Text)
puzzleOrbits = decodeOrbitsBlock <$> TIO.readFile orbitsFile


main11 :: IO ()
main11 = do
    putStrLn "Day 6, part 1"
    putStrLn $ "Hi, using: " ++ orbitsFile
    orbits <- puzzleOrbits
    let totalOrbits = countAllOrbits orbits
    putStrLn $ "Number of orbits = " ++ show totalOrbits
