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

module Day11.M22
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.Either       (either, fromLeft, fromRight, isLeft, lefts)
import           Data.Function     (on, (&))

import           Data.List         (foldl', intercalate, maximumBy, minimumBy,
                                    permutations, reverse)

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
opcodesFile = "files/11/opcodes.txt"


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


-- do a single paint (provide the input 0/1) and get the two outputs and return
-- them as a pair
runPaintCycle :: Machine -> Int -> (Machine, (Int,Int))
runPaintCycle m input =
    let m' = m { inList=inList m ++ [input] }
        m'' = fixRunWithState m'
        os = reverse $ outList m''
        newDir = head os
        painted = os !! 1
     in (m'', (painted, newDir))


data Direction = North | East | South | West deriving (Eq, Show)


data PaintState = PaintState { coords    :: H.HashMap (Int,Int) Int
                             , location  :: (Int,Int)
                             , direction :: Direction
                             , machine   :: Machine
                             }


runPainterStartOnWhite :: Machine -> PaintState
runPainterStartOnWhite m =
    let sqs = H.singleton (0,0) 1 :: H.HashMap (Int,Int) Int
        pos = (0,0)
        dir = North
     in runPainter' $ PaintState { coords=sqs
                                 , location=pos
                                 , direction=dir
                                 , machine=m
                                 }

-- run the painter until it ends
runPainter' :: PaintState -> PaintState
runPainter' ps =
    let sqs = coords ps
        currentColor = fromMaybe 0 (H.lookup (location ps) (coords ps))
        loc = location ps
        (m', (color, turn)) = runPaintCycle (machine ps) currentColor
        sqs' = H.insert loc color sqs
        dir' = turnWith turn (direction ps)
        loc' = moveBy dir' loc
        ps'  = PaintState { coords=sqs'
                          , location=loc'
                          , direction=dir'
                          , machine=m'
                          }
    in if not (ended m')
         then runPainter' ps'
         else ps'


turnWith :: Int -> Direction -> Direction
turnWith 0 North = West
turnWith 0 West  = South
turnWith 0 South = East
turnWith 0 East  = North
turnWith 1 North = East
turnWith 1 East  = South
turnWith 1 South = West
turnWith 1 West  = North
turnWith _ _     = error "invalid parameters to turnWith"


moveBy :: Direction -> (Int,Int) -> (Int,Int)
moveBy dir (x,y) =
    case dir of
        North -> (x, y-1)
        East  -> (x+1, y)
        South -> (x, y+1)
        West  -> (x-1, y)


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
      toChar _ = ' '


main22 :: IO ()
main22 = do
    putStrLn "AOC2019 Day 11 Part 2 - Space Police: the registration number"
    opcodes <- loadOpcodes
    let m = loadMachine opcodes
        ps = runPainterStartOnWhite m
        sqs = coords ps
    putStrLn $ draw sqs
