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

module Day05.M09
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

import           Control.Monad     (liftM)
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


type Memory = Array Int Int


data Machine = Machine { memory :: Memory
                       , ip     :: Int
                       } deriving Show


data Op = OpAdd | OpMult | OpInput | OpOutput | OpEnd deriving (Show, Eq)


data Mode = Immediate
          | Position
          | UnknownMode Int
          deriving Eq

instance Show Mode where
    show Immediate       = "IMM"
    show Position        = "POS"
    show (UnknownMode i) = "U" ++ show i


data Instruction = Instruction { opCode   :: Op
                               , size     :: Int
                               , modes    :: [Mode]
                               , location :: Int
                               }

instance Show Instruction where
    show i = "Instruction<" ++ show (opCode i)
          ++ ", size=" ++ show (size i)
          ++ ", modes=" ++ show (modes i)
          ++ ", at location=" ++ show (location i)
          ++ ">"


data MachineException = InvalidOpCode Int Int
                      | InvalidParameterMode Mode Int Int
                      | InvalidLocation Int
                      | InvalidInstructionPointer Int
                      | InvalidInstruction Instruction
                      | InvalidRead String
                      | ProgramEnded
                      | NullException
                      | MachineExceptions [MachineException]


instance Show MachineException where
    show (InvalidOpCode i pos) = "Invalid OpCode " ++ show i
                            ++ " at location " ++ show pos
    show (InvalidParameterMode m p pos) = "Invalid parameter mode ("
                                       ++ show m ++ ") for param "
                                       ++ show p ++ " at position: "
                                       ++ show pos
    show (InvalidLocation pos) = "Invalid location/position: " ++ show pos
    show (InvalidInstructionPointer pos) = "Invalid Instruction Pointer: " ++ show pos
    show (InvalidInstruction ix) = "Invalid Instruction: " ++ show ix
    show (MachineExceptions ms) = intercalate ", " $ map show ms
    show NullException  = "Null?"
    show ProgramEnded = "Ur, program ended"
    show (InvalidRead s) = s


intToOpCode :: Int -> Maybe Op
intToOpCode 1  = Just OpAdd
intToOpCode 2  = Just OpMult
intToOpCode 3  = Just OpInput
intToOpCode 4  = Just OpOutput
intToOpCode 99 = Just OpEnd
intToOpCode _  = Nothing


intToParamMode :: Int -> Maybe Mode
intToParamMode 0 = Just Position
intToParamMode 1 = Just Immediate
intToParamMode 2 = Nothing


opCodeToSize :: Op -> Int
opCodeToSize OpAdd    = 4
opCodeToSize OpMult   = 4
opCodeToSize OpInput  = 2
opCodeToSize OpOutput = 2
opCodeToSize OpEnd    = 1


decodeInstruction :: Machine -> Either MachineException Instruction
decodeInstruction m =
    let i = ip m
        mem = memory m
        (s,e) = bounds mem
        code = mem ! i
        maybeOp = intToOpCode $ code `mod` 100
        modeP1 = (code `mod` 1000) `div` 100
        modeP2 = (code `mod` 10000) `div` 1000
        modeP3 = (code `mod` 100000) `div` 10000
    in if i < s || i > e
         then Left $ InvalidInstructionPointer i
         else
            let op = maybe (Left $ InvalidOpCode code i) Right maybeOp
                p1Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP1) 1 i)
                               Right $ intToParamMode modeP1
                p2Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP2) 2 i)
                               Right $ intToParamMode modeP2
                p3Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP3) 3 i)
                               Right $ intToParamMode modeP3
                errors = [fromLeft NullException op | isLeft op]
                      ++ lefts [p1Mode, p2Mode, p3Mode]
            in if not (null errors)
                 then Left $ MachineExceptions errors
                 else do
                     op' <- op
                     p1Mode' <- p1Mode
                     p2Mode' <- p2Mode
                     p3Mode' <- p3Mode
                     return Instruction { opCode = op'
                                        , size   = opCodeToSize op'
                                        , modes  = [p1Mode', p2Mode', p3Mode']
                                        , location = i
                                        }


getTwoParams :: Machine -> Instruction -> Either MachineException (Int,Int)
getTwoParams m ix =
    let mem = memory m
        _modes = modes ix
        i = ip m
        p1 = getParam (head _modes) (i+1) mem  -- Either
        p2 = getParam (_modes !! 1) (i+2) mem  -- Either
        errors = lefts [p1,p2]
    in if not (null errors)
         then Left $ MachineExceptions errors
         else Right (fromRight ex p1,fromRight ex p2)
           where ex = error "Shouldn't be able to get here"


getTwoParamsM :: Members '[ Error MachineException
                          , State Machine
                          ] r
              => Instruction
              -> Sem r (Int, Int)
getTwoParamsM ix = do
    m <- get
    either raise return $ getTwoParams m ix
      where
          raise ex = throw $ MachineExceptions [InvalidInstruction ix, ex]


getParam :: Mode -> Int -> Memory -> Either MachineException Int
getParam mode pos mem =
    let (s,e) = bounds mem
        p1 = mem ! pos
     in if pos < s || pos > e
      then Left $ InvalidLocation pos
      else let imm = mem ! pos
            in case mode of
                Position -> if imm < s || imm > e
                              then Left $ InvalidLocation imm
                              else Right $ mem ! imm
                Immediate -> Right imm


getParamM :: Members '[ Error MachineException
                      , State Machine
                      ] r
              => Instruction
              -> Sem r Int
getParamM ix = do
    m <- get
    let p1 = getParam (head (modes ix)) (ip m +1) (memory m)  -- Either
    either throw return p1


exec :: Members '[ State Machine
                 , Error MachineException
                 , CP.Log String
                 , Teletype
                 ] r
        => Sem r ()
exec = do
    m <- get
    {-CP.log $ "IP=" ++ show (ip m)-}
    let i = decodeInstruction m
    case i of
        Left ex -> throw ex
        Right ix -> case opCode ix of
            OpAdd    -> doAction ix (+) *> updateIP ix *> exec
            OpMult   -> doAction ix (*) *> updateIP ix *> exec
            OpInput  -> inputOp  ix     *> updateIP ix *> exec
            OpOutput -> outputOp ix     *> updateIP ix *> exec
            OpEnd    -> return ()


doAction :: Members '[ State Machine
                     , Error MachineException
                     ] r
         => Instruction -> (Int -> Int -> Int)
         -> Sem r ()
doAction ix op = (uncurry op <$> getTwoParamsM ix) >>= storeAtM ix


updateIP :: Member (State Machine) r => Instruction -> Sem r ()
updateIP ix = modify (\m -> Machine { memory=memory m, ip=ip m + size ix })


loadMachine :: [Int] -> Machine
loadMachine xs = Machine { memory=listArray (0, length xs -1) xs, ip=0 }


storeAt :: Machine -> Int -> Int -> Machine
storeAt m i v = Machine { memory=mem', ip=ip m }
  where
      mem' = runST $ do
        arr <- thaw (memory m) :: ST s (STArray s Int Int)
        writeArray arr i v
        freeze arr


storeAtM :: Members '[ State Machine
                     , Error MachineException
                     ] r
         => Instruction
         -> Int
         -> Sem r ()
storeAtM ix v = do
    m <- get
    -- parameter mode is on param 3 and must be Position
    let mode = modes ix !! 2
        i = ip m + 3
        mem = memory m
        (s,e) = bounds mem
    if mode /= Position
      then throw $ InvalidParameterMode mode 3 i
      else if i < s || e > e
        then throw $ InvalidLocation i
        else let loc = mem ! i in
            if loc < s || loc > e
              then throw $ InvalidLocation loc
              else put $ storeAt m loc v


inputOp :: Members '[ State Machine
                    , Teletype
                    , Error MachineException
                    ] r
        => Instruction
        -> Sem r ()
inputOp ix = do
    writeTTY "Input >"
    vs <- readTTY
    case (readEither vs :: Either String Int) of
        Left s  -> throw $ InvalidRead s
        Right v -> storeAtM ix v


outputOp :: Members '[ State Machine
                     , Teletype
                     , Error MachineException
                     ] r
         => Instruction
         -> Sem r ()
outputOp ix = do
    p <- getParamM ix
    writeTTY $ show p

-- now two functions; one for running pure and the other for running in IO


run :: [Int] -> IO (Either MachineException ())
run opcodes = do
    --  Resolve the Sem monads with each interpret function to get the sitegen
    let m = loadMachine opcodes
    exec                                       -- [Teletype, State Machine, Log String, Embed IO, Error MachineException]
        & evalState m                          -- [Teletype, Log String, Embed IO, Error MachineException]
        & CP.runLogAction @IO logStringStdout  -- [Teletype, Embed IO, Error MachineException]
        & teletypeToIO                         -- [Embed IO, Error MachineException]
        & errorToIOFinal @MachineException     -- [Embed IO]
        & embedToFinal @IO
        & runFinal


-- TODO: write the pure version of run as runPure

opcodesFile = "files/05/opcodes.txt"


main09 :: IO ()
main09 = do
    putStrLn "AOC2019 Day 5 Part 1"
    block <- TIO.readFile opcodesFile
    let opcodes = (map (read . T.unpack) $ T.split (==',') block) :: [Int]
    res <- run opcodes
    either print return res
