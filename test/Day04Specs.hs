{-# LANGUAGE OverloadedStrings #-}
module Day04Specs where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import Day04.M07 (parsePassword, validatePassword, showPassword, incPassword, validatedPasswordsBetween)
import qualified Day04.M08 as M08



m07Specs :: Spec
m07Specs = --do

    describe "Day 04, part 1: Secure Container" $ do

        it "Should be able to parse a password." $
            parsePassword "123456" `shouldBe` [6,5,4,3,2,1]

        it "Password 111111 should be valid" $
            validatePassword (parsePassword "111111") `shouldBe` True

        it "Password 223450 should not be valid" $
            validatePassword (parsePassword "223450") `shouldBe` False

        it "Password 123789 should not be valid" $
            validatePassword (parsePassword "123789") `shouldBe` False

        it "Password 122345 should be valid" $
            validatePassword (parsePassword "122345") `shouldBe` True

        it "Increment 111111 should be 111112" $
            showPassword <$> incPassword (parsePassword "111111") `shouldBe` Just "111112"

        it "Increment 111111 should be 111112" $
            showPassword <$> incPassword (parsePassword "111111") `shouldBe` Just "111112"

        it "Increment 111119 should be 111122" $
            showPassword <$> incPassword (parsePassword "111119") `shouldBe` Just "111122"

        it "Increment 456789 should be 456799" $
            showPassword <$> incPassword (parsePassword "456789") `shouldBe` Just "456799"

        it "Increment 8999999 should be 999999" $
            showPassword <$> incPassword (parsePassword "899999") `shouldBe` Just "999999"

        it "Increment 134999 should be 135555" $
            showPassword <$> incPassword (parsePassword "134999") `shouldBe` Just "135555"

        it "Increment 999999 should be Nothing" $
            incPassword (parsePassword "999999") `shouldBe` Nothing

        -- finally we have the actual puzzle anser
        it "There should be 1864 passwords between 137683 and 596253" $
            length (validatedPasswordsBetween "137683" "596253") `shouldBe` 1864

m08Specs :: Spec
m08Specs = --do

    describe "Day 04, part 2: Secure Container - stronger validation" $ do

        it "Password 112233 should be valid" $
            M08.validatePassword (M08.parsePassword "112233") `shouldBe` True

        it "Password 123444 should not be valid" $
            M08.validatePassword (M08.parsePassword "123444") `shouldBe` False

        it "Password 111122 should be valid" $
            M08.validatePassword (M08.parsePassword "111122") `shouldBe` True

        it "There should be 1258 more strictly validated passwords between 137683 and 596253" $
            length (M08.validatedPasswordsBetween "137683" "596253") `shouldBe` 1258
