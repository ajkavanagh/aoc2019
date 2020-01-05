{-# LANGUAGE OverloadedStrings #-}
module Day14Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Day14.M27    as M27


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
