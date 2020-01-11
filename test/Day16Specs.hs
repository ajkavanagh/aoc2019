{-# LANGUAGE OverloadedStrings #-}
module Day16Specs where

import           Test.Hspec   (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Day16.M31    as M31


m31testIt :: T.Text -> Int -> String
m31testIt s n = let digits = M31.loadDigits s
                 in concatMap show $ take 8 $ M31.doPhases digits n


m31Specs :: Spec
m31Specs = --do

    describe "Day 16, part 1: Flawed Frequency Transmission" $ do

        it "12345678, 4 phases, should be 01029498" $
            m31testIt "12345678" 4 `shouldBe` "01029498"

        it "80871224585914546619083218645595, 100 phases, should be 24176176" $
            m31testIt "80871224585914546619083218645595" 100 `shouldBe` "24176176"

        it "19617804207202209144916044189917, 100 phases, should be 73745418" $
            m31testIt "19617804207202209144916044189917" 100 `shouldBe` "73745418"

        it "69317163492948606335995924319873, 100 phases, should be 52432133" $
            m31testIt "69317163492948606335995924319873" 100 `shouldBe` "52432133"
