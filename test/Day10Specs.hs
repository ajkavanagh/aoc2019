{-# LANGUAGE OverloadedStrings #-}
module Day10Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Data.Vector  as V

import qualified Day10.M19    as M19
import qualified Day10.M20    as M20


testIt :: T.Text -> ((Int,Int),Int)
testIt tx = M19.pickMaximum $ M19.tagVisbleFor $ M19.asteroidCoords $ M19.loadMap tx

m19Specs :: Spec
m19Specs = --do

    describe "Day 10, part 1: Monitoring Station" $ do

        it "testMap1 should be ((3,4),8)" $
            testIt M19.testMap1 `shouldBe` ((3,4),8)

        it "testMap2 should be ((5,8),33)" $
            testIt M19.testMap2 `shouldBe` ((5,8),33)

        it "testMap3 should be ((1,2),35)" $
            testIt M19.testMap3 `shouldBe` ((1,2),35)

        it "testMap4 should be ((6,3),41)" $
            testIt M19.testMap4 `shouldBe` ((6,3),41)

        it "testMap5 should be ((11,13),210)" $
            testIt M19.testMap5 `shouldBe` ((11,13),210)

m20Specs :: Spec
m20Specs = --do

    describe "Day 10, part 2: the rotating lazer  testmap5, loc (11,13)" $ do

        it "The 1st asteroid to be vaporized is at 11,12." $
            snd (M20.solveForTestMap5 V.! 0) `shouldBe` (11,12)

        it "The 2nd asteroid to be vaporized is at 12,1." $
            snd (M20.solveForTestMap5 V.! 1) `shouldBe` (12,1)

        it "The 3rd asteroid to be vaporized is at 12,2." $
            snd (M20.solveForTestMap5 V.! 2) `shouldBe` (12,2)

        it "The 10th asteroid to be vaporized is at 12,8." $
            snd (M20.solveForTestMap5 V.! 9) `shouldBe` (12,8)

        it "The 20th asteroid to be vaporized is at 16,0." $
            snd (M20.solveForTestMap5 V.! 19) `shouldBe` (16,0)

        it "The 50th asteroid to be vaporized is at 16,9." $
            snd (M20.solveForTestMap5 V.! 49) `shouldBe` (16,9)

        it "The 199th asteroid to be vaporized is at 9,6." $
            snd (M20.solveForTestMap5 V.! 198) `shouldBe` (9,6)

        it "The 200th asteroid to be vaporized is at 8,2." $
            snd (M20.solveForTestMap5 V.! 199) `shouldBe` (8,2)

        it "The 201st asteroid to be vaporized is at 10,9." $
            snd (M20.solveForTestMap5 V.! 200) `shouldBe` (10,9)

        it "The 299th and final asteroid to be vaporized is at 11,1." $
            snd (M20.solveForTestMap5 V.! 298) `shouldBe` (11,1)
