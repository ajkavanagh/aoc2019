{-# LANGUAGE OverloadedStrings #-}
module Day07Specs where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import qualified Day07.M13 as M13



m13Specs :: Spec
m13Specs = --do

    describe "Day 07, part 1: Amplification Circuit" $ do

        it "The 5 connected amps with testset 1 and input '43210' should be '43210'" $
            M13.combined M13.loadTest1 "43210" `shouldBe` "43210"

        it "The 5 connected amps with testset 2 and input '01234' should be '54321'" $
            M13.combined M13.loadTest2 "01234" `shouldBe` "54321"

        it "The 5 connected amps with testset 3 and input '10432' should be '65210'" $
            M13.combined M13.loadTest3 "10432" `shouldBe` "65210"
