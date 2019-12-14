module Day01Specs where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import Day01.M01 (calc)
import Day01.M02 (rcalc)


m01Specs :: Spec
m01Specs = --do

    describe "Day 01, part1: calc should perform x `div` 3 - 2" $ do

        it "A module of mass 12 requires 2 fuel" $
            calc 12 `shouldBe` 2

        it "For a mass of 14 also requires 2 fuel" $
            calc 14 `shouldBe` 2

        it "For a mass of 1969, the fuel required is 654" $
            calc 1969 `shouldBe` 654

        it "For a mass of 100756, the fuel required is 33583" $
            calc 100756 `shouldBe` 33583


m02Specs :: Spec
m02Specs =

    describe "Day 01, part 2: calc recursively adds in fuel for fuel." $ do

        it "A module of mass 12 only requires 2 fuel" $
            rcalc 12 `shouldBe` 2

        it "A module of mass 1969 recursively requires fuel 966" $
            rcalc 1969 `shouldBe` 966

        it "A module of mass 100756 recursively requires fuel 50346" $
            rcalc 100756 `shouldBe` 50346
