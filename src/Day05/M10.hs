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

module Day05.M10
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

import           Data.List         (intercalate)

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
                                    doAction, inputOp, opCode, outputOp, jumpOp,
                                    runWith, ip, memory)


exec :: Members '[ State Machine
                 , Error MachineException
                 , CP.Log String
                 , Teletype
                 ] r
        => Sem r ()
exec = do
    m <- get @Machine
    {-CP.log $ "IP=" ++ show (ip m)-}
    CP.log $ "mem = " ++ show (memory m)
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
                OpEnd       -> return ()
  where
      toBool True = 1
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
intToOpCode 99 = Just OpEnd
intToOpCode _  = Nothing


intToParamMode :: Int -> Maybe Mode
intToParamMode 0 = Just Position
intToParamMode 1 = Just Immediate
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


-- now two functions; one for running pure and the other for running in IO

run :: [Int] -> IO (Either MachineException ())
run opcodes = runWith opcodes exec


-- TODO: write the pure version of run as runPure

opcodesFile = "files/05/opcodes.txt"


main10 :: IO ()
main10 = do
    putStrLn "AOC2019 Day 5 Part 2"
    block <- TIO.readFile opcodesFile
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
    res <- run opcodes
    either print return res
