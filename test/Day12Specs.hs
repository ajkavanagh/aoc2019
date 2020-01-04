{-# LANGUAGE OverloadedStrings #-}
module Day12Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Data.Vector  as V

import qualified Day12.M23    as M23
import qualified Day12.M24    as M24


m23testIt :: [T.Text] -> Int -> Int
m23testIt ts steps = let ms = M23.loadTestSet ts
                         ms' = M23.doSteps steps ms
                         e = M23.moonsEnergy ms'
                      in e


m23Specs :: Spec
m23Specs = --do

    describe "Day 12, part 1: The N-Body Problem" $ do

        it "testSet1 energy after 10 steps should be 179" $
            m23testIt M23.testSet1 10 `shouldBe` 179

        it "testSet2 energy after 100 steps should be 1940" $
            m23testIt M23.testSet2 100 `shouldBe` 1940


m24testIt :: [T.Text] -> Maybe Int
m24testIt ts = M24.computeCycleFor (M24.loadTestSet ts)

m24Specs :: Spec
m24Specs = --do

    describe "Day 12, part 2: The N-Body Problem - cycles" $ do

        it "testSet1 should have a cycle length of 2772" $
            m24testIt M24.testSet1 `shouldBe` Just 2772

        it "testSet2 should have a cycle length of 4686774924" $
            m24testIt M24.testSet2 `shouldBe` Just 4686774924
