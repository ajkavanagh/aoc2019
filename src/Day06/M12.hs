{-# LANGUAGE OverloadedStrings    #-}
module Day06.M12 where



import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import qualified Data.List as L
import Data.Tuple.Extra (both)

import qualified Data.HashMap.Lazy as H
import qualified Data.HashSet as HS

orbitsFile = "files/06/orbits.txt"


type OrbitsMap = H.HashMap T.Text T.Text


decodeOrbitsBlock :: T.Text -> OrbitsMap
decodeOrbitsBlock txt =
    let ts = T.lines txt
        lx = map (T.split (==')')) ts
        px = map makePair lx
    in H.fromList px
   where makePair :: [T.Text] -> (T.Text, T.Text)
         makePair [x,y] = (y, x)
         makePair _ = error "Problem with list?"


countOrbitsFor :: OrbitsMap -> T.Text -> Int
countOrbitsFor m k = L.length $ orbitsToCentre m k


orbitsToCentre :: OrbitsMap -> T.Text -> [T.Text]
orbitsToCentre m = L.unfoldr follow
  where
      follow :: T.Text -> Maybe (T.Text, T.Text)
      follow t = H.lookup t m >>= \v -> Just (v,v)


countAllOrbits :: OrbitsMap -> Int
countAllOrbits m = sum $ map (countOrbitsFor m) $ HS.toList $ H.keysSet m


-- find differing orbit strings (we have to reverse them, then pair them,
-- and then find the first pair that are different, but remember the pair
-- before them.  Then work down one set of pairs and back up the other
-- Note they may be differing lengths, and we might not find the solution -- but
-- the puzzle is set up so that we will.
-- The number of orbits is the sum of the remaining lengths
findTransfersFor :: [T.Text] -> [T.Text] -> Int
findTransfersFor os1 os2 =
    let (ros1, ros2) = both reverse (os1, os2)
        ps = zip ros1 ros2
        commonCount = length $ takeWhile (uncurry (==)) ps
        (os1', os2') = both (drop commonCount) (ros1, ros2)
     in sum $ map length [os1',os2']

calcTransfersFor :: OrbitsMap -> T.Text -> T.Text -> Int
calcTransfersFor m x y =
    let xs = orbitsToCentre m x
        ys = orbitsToCentre m y
    in findTransfersFor xs ys


testSet :: OrbitsMap
testSet =
    let orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
     in decodeOrbitsBlock orbits


puzzleOrbits :: IO OrbitsMap
puzzleOrbits = decodeOrbitsBlock <$> TIO.readFile orbitsFile


main12 :: IO ()
main12 = do
    putStrLn "Day 6, part 2"
    putStrLn $ "Hi, using: " ++ orbitsFile
    orbits <- puzzleOrbits
    let transfers = calcTransfersFor orbits "YOU" "SAN"
    putStrLn $ "Number of transfers = " ++ show transfers
