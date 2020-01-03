{-# LANGUAGE OverloadedStrings #-}
module Day12Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Data.Vector  as V

import qualified Day12.M23    as M23


testIt :: [T.Text] -> Int -> Int
testIt ts steps = let ms = M23.loadTestSet ts
                      ms' = M23.doSteps steps ms
                      e = M23.moonsEnergy ms'
                   in e


m23Specs :: Spec
m23Specs = --do

    describe "Day 12, part 1: The N-Body Problem" $ do

        it "testSet1 energy after 10 steps should be 179" $
            testIt M23.testSet1 10 `shouldBe` 179

        it "testSet2 energy after 100 steps should be 1940" $
            testIt M23.testSet2 100 `shouldBe` 1940
