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

module Day13.M25
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.Either       (either, fromLeft, fromRight, isLeft, lefts)
import           Data.Function     (on, (&))

import           Data.List         (foldl', intercalate, maximumBy, minimumBy,
                                    permutations, reverse)
import           Data.List.Split   (chunksOf)

import           Data.Maybe        (fromMaybe)

import qualified Data.HashMap.Lazy as H

-- for Polysemy
import           Colog.Core        (logStringStdout)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Member, Members, Sem, embedToFinal,
                                    runFinal)
import           Polysemy.Error    (Error, errorToIOFinal, throw)
import           Polysemy.State    (State, evalState, get, modify, put)

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
                                    runWithPureState, runWithState)



exec :: Members '[ State Machine
                 , Error MachineException
                 , CP.Log String
                 , Teletype
                 ] r
        => Sem r ()
exec = do
    m <- get @Machine
    let i = decodeInstruction m
    case i of
        Left ex -> throw ex
        Right ix -> do
            CP.log $ show ix
            case opCode ix of
                OpAdd       -> doAction ix (+)  *> exec
                OpMult      -> doAction ix (*)  *> exec
                OpInput     -> inputOpState ix >>= \yield ->
                    if yield then pure () else exec
                OpOutput    -> outputOpState ix      *> exec
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
opcodesFile = "files/13/opcodes.txt"


loadOpcodes ::  IO [Int]
loadOpcodes = map (read . T.unpack) . T.split (==',')
          <$> TIO.readFile opcodesFile


-- and finally some runners for the program


-- initialise a Machine with opcodes and an input
initialiseMachineWith :: [Int] -> Int -> Machine
initialiseMachineWith opcodes input =
    let m = loadMachine opcodes in m { inList=[input] }


runPure :: [Int] -> [String] -> Either MachineException ([String], ())
runPure opcodes input = runWithPure opcodes input exec


runIO :: [Int] -> IO (Either MachineException ())
runIO opcodes = runWith opcodes exec


fixRunWithState :: Machine -> Machine
fixRunWithState m =
    let res = runWithPureState [] exec m :: Either MachineException ([String], (Machine, ()))
     in case res of
        Right (_, (m', _)) -> m'
        -- this is also horrible; TODO work out how to fix this so we don't have
        -- to use fail
        Left ex            -> error $ show ex


-- Run the game engine until it ends; there should be no input.
runGameEngine :: Machine -> (Machine, [Int])
runGameEngine m =
    let m' = fixRunWithState m
        os = outList m'
     in (m', os)


mapCoords :: [Int] -> [((Int,Int), Int)]
mapCoords os = map go $ chunksOf 3 os
  where
      go :: [Int] -> ((Int,Int), Int)
      go [x,y,t] = ((x,y),t)
      go _ = error "Not a chunk of 3???"


loadCoords :: [Int] -> H.HashMap (Int,Int) Int
loadCoords = H.fromList . mapCoords


getDimens :: H.HashMap (Int,Int) Int -> ((Int,Int),(Int,Int))
getDimens m =
    let l = H.toList m
        matchX ((x,_),_) = x
        getX = compare `on` matchX
        matchY ((_,y),_) = y
        getY = compare `on` matchY
        ((xmin,_),_) = minimumBy getX l
        ((xmax,_),_) = maximumBy getX l
        ((_,ymin),_) = minimumBy getY l
        ((_,ymax),_) = maximumBy getY l
    in ((xmin,xmax),(ymin,ymax))


draw :: H.HashMap (Int,Int) Int -> String
draw m =
    let ((xmin,xmax),(ymin,ymax)) = getDimens m
     in unlines [[ toChar (fromMaybe 0 (H.lookup (x,y) m))
                 | x <- [xmin..xmax]]
                 | y <- [ymin..ymax]]
  where
      toChar 1 = '#'
      toChar 2 = '='
      toChar 3 = '-'
      toChar 4 = 'o'
      toChar _ = ' '


countBlockTiles :: H.HashMap (Int,Int) Int -> Int
countBlockTiles = length . filter (==2) . map snd . H.toList

main25 :: IO ()
main25 = do
    putStrLn "AOC2019 Day 13 Part 1 - Care Package"
    opcodes <- loadOpcodes
    let m = loadMachine opcodes
        (m', ps) = runGameEngine m
        sqs = loadCoords ps
    putStrLn $ draw sqs
    putStrLn $ "\nThe number of block tiles is: " ++ show (countBlockTiles sqs)
