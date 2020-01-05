{-# LANGUAGE OverloadedStrings #-}
module Day14Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Day14.M27    as M27
import qualified Day14.M28    as M28


m27testIt :: [T.Text] -> Int
m27testIt ts = let (Right rts) = M27.loadTestSet ts
                in M27.chemMade "ORE" $ M27.consume (1, "FUEL") M27.newReactor rts


m27Specs :: Spec
m27Specs = --do

    describe "Day 14, part 1: Space Stoichiometry" $ do

        it "testSet1 should consume 31 ORE" $
            m27testIt M27.testSet1 `shouldBe` 31

        it "testSet2 should consume 165 ORE" $
            m27testIt M27.testSet2 `shouldBe` 165

        it "testSet3 should consume 13312 ORE" $
            m27testIt M27.testSet3 `shouldBe` 13312

        it "testSet4 should consume 180697 ORE" $
            m27testIt M27.testSet4 `shouldBe` 180697

        it "testSet5 should consume 2210736 ORE" $
            m27testIt M27.testSet5 `shouldBe` 2210736


m28testIt :: [T.Text] -> Int
m28testIt ts = let (Right rts) = M28.loadTestSet ts
                in M28.findFuelForOneT rts


m28Specs :: Spec
m28Specs = --do

    describe "Day 14, part 1: Space Stoichiometry: fuel for 1T ORE" $ do

        it "testSet3 should provide 82892753 FUEL for 1_000_000_000 ORE" $
            m28testIt M28.testSet3 `shouldBe` 82892753

        it "testSet4 should provide 5586022 FUEL for 1_000_000_000 ORE" $
            m28testIt M28.testSet4 `shouldBe` 5586022

        it "testSet5 should provide 460664 FUEL for 1_000_000_000 ORE" $
            m28testIt M28.testSet5 `shouldBe` 460664
