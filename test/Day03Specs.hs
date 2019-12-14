{-# LANGUAGE OverloadedStrings #-}
module Day03Specs where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T

import Day03.M05 (nearest)
import Day03.M06 (nearest2)


wiresFile :: String
wiresFile = "files/03/wires.txt"


m05Specs :: Spec
m05Specs = --do

    describe "Day 03, part 1: Crossed Wires" $ do

        it "With U7,R6,D4,L4 and R8,U5,L5,D3 result should be 6" $
            nearest "U7,R6,D4,L4" "R8,U5,L5,D3" `shouldBe` 6

        it "With R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83 should be 159" $
            nearest "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                    "U62,R66,U55,R34,D71,R55,D58,R83"
                    `shouldBe` 159

        it "With R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 should be 135" $
            nearest "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                    `shouldBe` 135

        it "The solution for the wires file is 280" $ do
            wireText <- TIO.readFile wiresFile
            let lines = T.lines wireText
            nearest (head lines) (lines !! 1) `shouldBe` 280


m06Specs :: Spec
m06Specs = --do

    describe "Day 03, part 2: Crossed Wires, but by steps" $ do

        it "With U7,R6,D4,L4 and R8,U5,L5,D3 result should be 30" $
            nearest2 "U7,R6,D4,L4" "R8,U5,L5,D3" `shouldBe` 30

        it "With R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83 should be 610" $
            nearest2 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                     "U62,R66,U55,R34,D71,R55,D58,R83"
                     `shouldBe` 610

        it "With R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 should be 410" $
            nearest2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                     "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                     `shouldBe` 410

        it "The solution for the wires file is 10554" $ do
            wireText <- TIO.readFile wiresFile
            let lines = T.lines wireText
            nearest2 (head lines) (lines !! 1) `shouldBe` 10554
