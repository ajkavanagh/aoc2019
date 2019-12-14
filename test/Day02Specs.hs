module Day02Specs where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import Day02.M03 (run)
import Day02.M04 (findSolution)


opcodesFile = "files/02/opcodes.txt"


m03Specs :: Spec
m03Specs = --do

    describe "Day 02, part 1: 1202 Program Alarm" $ do

        it "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)" $
            run [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

        it "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)" $
            run [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

        it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)" $
            run [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]

        it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
            run [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]


m04Specs :: Spec
m04Specs = --do

    describe "Day 02, part 2: find the solution for 19690720" $ do

        it "Solution for 19690720 should be 8976" $ do
            block <- TIO.readFile opcodesFile
            findSolution block 19690720 `shouldBe` 8976


