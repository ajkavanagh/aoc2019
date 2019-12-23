{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Day07.M13
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.Either       (either, fromLeft, fromRight, isLeft, lefts)
import           Data.Function     ((&))

import           Data.Array        (Array, bounds, listArray, (!))
import           Data.Array.MArray (freeze, newListArray, thaw, writeArray)
import           Data.Array.ST     (STArray)

import           Data.List         (foldl', intercalate, maximumBy,
                                    permutations)

import           Control.Monad.ST  (ST, runST)

-- for Polysemy
import           Colog.Core        (logStringStdout)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Member, Members, Sem, embedToFinal,
                                    runFinal)
import           Polysemy.Error    (Error, errorToIOFinal, throw)
import           Polysemy.State    (State, evalState, get, modify, put)

-- for safe reading
import           Text.Read         (readEither)


-- Effect for input & output
import           Teletype          (Teletype, readTTY, teletypeToIO, writeTTY)


-- OpCode Machine
import           OpCodeMachine     (Instruction, Machine, MachineException (..),
                                    Mode (..), Op (..), decodeInstructionUsing,
                                    doAction, inputOp, ip, jumpOp, opCode,
                                    outputOp, runWith, runWithPure)


-- use the opcode machine from Day 05, part 2
import           Day05.M10         (exec)


opcodesFile = "files/07/opcodes.txt"




loadOpcodes ::  IO [Int]
loadOpcodes = map (read . T.unpack) . T.split (==',')
          <$> TIO.readFile opcodesFile

loadTest1 :: [Int]
loadTest1 =
    let block = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
     in map (read . T.unpack) . T.split (==',') $ block

loadTest2 :: [Int]
loadTest2 =
    let block = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
     in map (read . T.unpack) . T.split (==',') $ block

loadTest3 :: [Int]
loadTest3 =
    let block = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,"
             <> "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
     in map (read . T.unpack) . T.split (==',') $ block


{-runPure :: [Int] -> [String] -> Either MachineException String-}
runPure opcodes input = runWithPure opcodes input exec


run opcodes = runWith opcodes exec


-- runs the 5 opcode machines with the input string split into chars and a
-- "0" fed to the first opcode machine.  The output is the final thing printed
-- as a string.  This uses the runPure interpreter which pretends to be actual
-- input/output.
combined opcodes input = foldl' runner "0" (splitString input)
  where
      splitString = map (: [])
      runner :: String -> String -> String
      runner value phase = let res = runPure opcodes [phase, value]
                           in case res of
                               Left ex -> error $ show ex
                               Right r -> last (fst r)


findHighest :: IO String
findHighest = do
    let perms = map concat $ permutations $ map (: []) "01234"
    opcodes <- loadOpcodes
    let combs = [(combined opcodes phases, phases) | phases <- perms]
        max_comb = maximumBy (\x y -> (read (fst x) :: Int) `compare` (read (fst y) :: Int)) combs
    return $ fst max_comb ++ " from " ++ snd max_comb

main13 :: IO ()
main13 = do
    putStrLn "AOC2019 Day 7 Part 1"
    res <- findHighest
    print res
