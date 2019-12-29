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

module Day09.M17
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

import           Control.Monad     (foldM)
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
                                    Mode (..), Op (..), adjustRelBaseOp,
                                    appendToMachineInput,
                                    decodeInstructionUsing, doAction, endOp,
                                    ended, inList, inputOp, inputOpState, ip,
                                    jumpOp, loadMachine, memory, opCode,
                                    outList, outputOp, outputOpState,
                                    peekMachineOutput, runWith, runWithPure,
                                    runWithState)



-- the day 9 version, which now includes the relative base instruction, but runs
-- input normally without exiting.
exec :: Members '[ State Machine
                 , Error MachineException
                 , CP.Log String
                 , Teletype
                 ] r
        => Sem r ()
exec = do
    m <- get @Machine
    {-CP.log $ "IP=" ++ show (ip m)-}
    {-CP.log $ "machine = " ++ show m-}
    let i = decodeInstruction m
    case i of
        Left ex -> throw ex
        Right ix -> do
            CP.log $ show ix
            case opCode ix of
                OpAdd       -> doAction ix (+)  *> exec
                OpMult      -> doAction ix (*)  *> exec
                OpInput     -> inputOp  ix      *> exec
                OpOutput    -> outputOp ix      *> exec
                OpJumpTrue  -> jumpOp   ix (/=0) *> exec
                OpJumpFalse -> jumpOp   ix (==0) *> exec
                OpLessThan  -> doAction ix lt   *> exec
                OpEquals    -> doAction ix eq   *> exec
                OpEnd       -> endOp
                OpRBAdj     -> adjustRelBaseOp ix  *> exec
  where
      toBool True  = 1
      toBool False = 0
      lt x y = toBool $ x < y
      eq x y = toBool $ x == y


decodeInstruction :: Machine -> Either MachineException Instruction
decodeInstruction = decodeInstructionUsing intToOpCode intToParamMode opCodeToSize


intToOpCode :: Int -> Maybe Op
intToOpCode 1  = Just OpAdd
intToOpCode 2  = Just OpMult
intToOpCode 3  = Just OpInput
intToOpCode 4  = Just OpOutput
intToOpCode 5  = Just OpJumpTrue
intToOpCode 6  = Just OpJumpFalse
intToOpCode 7  = Just OpLessThan
intToOpCode 8  = Just OpEquals
intToOpCode 9  = Just OpRBAdj
intToOpCode 99 = Just OpEnd
intToOpCode _  = Nothing


intToParamMode :: Int -> Maybe Mode
intToParamMode 0 = Just Position
intToParamMode 1 = Just Immediate
intToParamMode 2 = Just Relative
intToParamMode _ = Nothing


opCodeToSize :: Op -> Int
opCodeToSize OpAdd       = 4
opCodeToSize OpMult      = 4
opCodeToSize OpInput     = 2
opCodeToSize OpOutput    = 2
opCodeToSize OpJumpTrue  = 3
opCodeToSize OpJumpFalse = 3
opCodeToSize OpLessThan  = 4
opCodeToSize OpEquals    = 4
opCodeToSize OpEnd       = 1
opCodeToSize OpRBAdj     = 2


-- now the test parts
opcodesFile = "files/09/opcodes.txt"


loadOpcodes ::  IO [Int]
loadOpcodes = map (read . T.unpack) . T.split (==',')
          <$> TIO.readFile opcodesFile

-- a program that copies itself to the output
loadTest1 :: [Int]
loadTest1 =
    let block = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
     in map (read . T.unpack) . T.split (==',') $ block

-- A program that outputs a really big number
loadTest2 :: [Int]
loadTest2 =
    let block = "1102,34915192,34915192,7,4,7,99,0"
     in map (read . T.unpack) . T.split (==',') $ block

-- A program that outputs the number in the middle
loadTest3 :: [Int]
loadTest3 =
    let block = "104,1125899906842624,99"
     in map (read . T.unpack) . T.split (==',') $ block

-- and finally some runners for the program

runPure :: [Int] -> [String] -> Either MachineException ([String], ())
runPure opcodes input = runWithPure opcodes input exec


runIO :: [Int] -> IO (Either MachineException ())
runIO opcodes = runWith opcodes exec


main17 :: IO ()
main17 = do
    putStrLn "AOC2019 Day 9 Part 1 - Sensor Boost opcode programs"
    opcodes <- loadOpcodes
    res <- runIO opcodes
    print res
