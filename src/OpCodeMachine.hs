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

module OpCodeMachine
    {-( -}
    {-)-}
      where


import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.Either       (either, fromLeft, fromRight, isLeft, lefts,
                                    rights)
import           Data.Function     ((&))

import           Data.Array        (Array, bounds, listArray, (!))
import           Data.Array.MArray (freeze, newListArray, thaw, writeArray)
import           Data.Array.ST     (STArray)

import           Data.List         (intercalate)

import           Control.Monad.ST  (ST, runST)

-- for Polysemy
import           Colog.Core        (logStringStdout)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Member, Members, Sem, embedToFinal, run,
                                    runFinal)
import           Polysemy.Error    (Error, errorToIOFinal, runError, throw)
import           Polysemy.Output   (ignoreOutput)
import           Polysemy.State    (State, evalState, get, modify, put,
                                    runState)

-- for safe reading
import           Text.Read         (readEither)


-- Effect for input & output
import           Teletype          (Teletype, readTTY, runTeletypePure,
                                    teletypeToIO, writeTTY)


-- for expandable 'memory'
import qualified Memory            as M


data Machine = Machine { memory  :: M.MemoryInt
                       , ip      :: Int
                       , relBase :: Int
                       , ended   :: Bool
                       , inList  :: [Int]
                       , outList :: [Int]
                       } deriving Show


data Op = OpAdd
        | OpMult
        | OpInput
        | OpOutput
        | OpEnd
        | OpJumpTrue
        | OpJumpFalse
        | OpLessThan
        | OpEquals
        | OpRBAdj
        deriving (Show, Eq)


data Mode = Immediate
          | Position
          | Relative
          | UnknownMode Int
          deriving Eq


instance Show Mode where
    show Immediate       = "IMM"
    show Position        = "POS"
    show Relative        = "REL"
    show (UnknownMode i) = "U" ++ show i


data Instruction = Instruction { opCode   :: Op
                               , size     :: Int
                               , modes    :: [Mode]
                               , params   :: [Int]
                               , location :: Int
                               }

instance Show Instruction where
    show ix = show (location ix) ++ ": " ++ case opCode ix of
        OpAdd       -> "ADD  " ++ showParam ix 0 ++ "+" ++ showParam ix 1 ++ " -> " ++ showParam ix 2
        OpMult      -> "MULT " ++ showParam ix 0 ++ "*" ++ showParam ix 1 ++ " -> " ++ showParam ix 2
        OpInput     -> "IN   " ++ showParam ix 0
        OpOutput    -> "OUT  " ++ showParam ix 0
        OpEnd       -> "END"
        OpJumpTrue  -> "JIT  " ++ showParam ix 0 ++ "/= 0 ip=" ++ showParam ix 1
        OpJumpFalse -> "JIF  " ++ showParam ix 0 ++ "== 0 ip=" ++ showParam ix 1
        OpLessThan  -> "LT   " ++ showParam ix 0 ++ " < " ++ showParam ix 1 ++ " -> " ++ showParam ix 2
        OpEquals    -> "EQ   " ++ showParam ix 0 ++ " == " ++ showParam ix 1 ++ " -> " ++ showParam ix 2
        OpRBAdj     -> "RBA  " ++ showParam ix 0


showParam :: Instruction -> Int -> String
showParam ix i =
    let _mode = modes ix !! i
        v     = params ix !! i
    in case _mode  of
        Immediate -> show v
        Position  -> "(" ++ show v ++ ")"
        Relative  -> "[RB" ++ (if v < 0 then "-" else "+") ++ show (abs v) ++ "]"


data MachineException = InvalidOpCode Int Int
                      | InvalidParameterMode Mode Int Int
                      | InvalidLocation String Int
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
    show (InvalidLocation msg pos) = "Invalid location/position: " ++ msg ++ ": " ++ show pos
    show (InvalidInstructionPointer pos) = "Invalid Instruction Pointer: " ++ show pos
    show (InvalidInstruction ix) = "Invalid Instruction: " ++ show ix
    show (MachineExceptions ms) = intercalate ", " $ map show ms
    show NullException  = "Null?"
    show ProgramEnded = "Ur, program ended"
    show (InvalidRead s) = s


loadMachine :: [Int] -> Machine
loadMachine xs = Machine { memory=M.defaultLoadMemoryFromList (0 :: Int) xs
                         , ip=0
                         , relBase=0
                         , ended=False
                         , inList=[]
                         , outList=[]
                         }


storeAtM :: Members '[ State Machine
                     , CP.Log String
                     , Error MachineException
                     ] r
         => Instruction
         -> Int -- parameter number (1,2 or 3)
         -> Int -- value
         -> Sem r ()
storeAtM ix p v = do
    m <- get @Machine
    -- parameter mode is on param p and must be Position or Relative
    let mode = modes ix !! (p -1)
        i = ip m + p
        pval = params ix !! (p -1)
        mem = memory m
    case mode of
        Immediate -> throw $ InvalidParameterMode mode p i
        Position -> case M.store mem pval v of
            Left ex -> throw $ InvalidLocation ("storeAtM Position store " ++ show ex) pval
            Right mem' ->  do
                CP.log $ "storeAtM " ++ show ix ++ " pos:" ++ show pval ++ " value:" ++ show v
                put @Machine m {memory=mem'}
        Relative -> let relLoc = relBase m + pval
                     in case M.store mem relLoc v of
                        Left ex -> throw $ InvalidLocation ("storeAtM:relative " ++ show ex) relLoc
                        Right mem' -> do
                            CP.log $ "storeAtM(rel) " ++ show ix ++ " pos:" ++ show pval ++ " value:" ++ show v
                            put @Machine m {memory=mem'}


decodeInstructionUsing :: (Int -> Maybe Op)    -- int to opcode
                       -> (Int -> Maybe Mode)  -- int to the parameter mode
                       -> (Op -> Int)          -- Opcode to size of instruction
                       -> Machine              -- Machine to use.
                       -> Either MachineException Instruction
decodeInstructionUsing iToOpCode iToParamMode opToInt m =
    let i = ip m
        mem = memory m
     in case getMemLoc m i of
         Left _ -> Left $ InvalidInstructionPointer i
         Right code ->
             let maybeOp = iToOpCode $ code `mod` 100
                 modeP1 = (code `mod` 1000) `div` 100
                 modeP2 = (code `mod` 10000) `div` 1000
                 modeP3 = (code `mod` 100000) `div` 10000
                 op = maybe (Left $ InvalidOpCode code i) Right maybeOp
                 p1Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP1) 1 i)
                                Right $ iToParamMode modeP1
                 p2Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP2) 2 i)
                                Right $ iToParamMode modeP2
                 p3Mode = maybe (Left $ InvalidParameterMode (UnknownMode modeP3) 3 i)
                                Right $ iToParamMode modeP3
                 errors = [fromLeft NullException op | isLeft op]
                       ++ lefts [p1Mode, p2Mode, p3Mode]
               in if not (null errors)
                    then Left $ MachineExceptions errors
                    else do
                        op' <- op
                        p1Mode' <- p1Mode
                        p2Mode' <- p2Mode
                        p3Mode' <- p3Mode
                        let count = opToInt op'
                            px = map (getMemLoc m) [i+1..i+count-1]
                            pxErrors = lefts px
                        if not (null pxErrors)
                          then Left $ MachineExceptions pxErrors
                          else return Instruction { opCode = op'
                                                  , size   = opToInt op'
                                                  , modes  = [p1Mode', p2Mode', p3Mode']
                                                  , params = rights px
                                                  , location = i
                                                  }


getMemLoc :: Machine -> Int -> Either MachineException Int
getMemLoc m p = case M.fetch (memory m) p of
    Left ex -> Left $ InvalidLocation ("getMemLoc " ++ show ex) p
    Right v -> Right v


getTwoParams :: Machine -> Instruction -> Either MachineException (Int,Int)
getTwoParams m ix =
    let p1 = getParamIx 1 ix m  -- Either
        p2 = getParamIx 2 ix m  -- Either
        errors = lefts [p1,p2]
    in if not (null errors)
         then Left $ MachineExceptions errors
         else Right (fromRight ex p1,fromRight ex p2)
           where ex = error "Shouldn't be able to get here"


getTwoParamsM :: Members '[ Error MachineException
                          , CP.Log String
                          , State Machine
                          ] r
              => Instruction
              -> Sem r (Int, Int)
getTwoParamsM ix = do
    m <- get @Machine
    let ps = getTwoParams m ix
    CP.log $ show ps
    either raise return ps
      where
          raise ex = throw $ MachineExceptions [InvalidInstruction ix, ex]


getParamIx :: Int -> Instruction -> Machine -> Either MachineException Int
getParamIx p ix m =
    let pval = params ix !! (p -1)  -- params numbered 1,2,3
        mode = modes ix !! (p -1)   -- get the mode
     in case mode of
         Immediate -> Right pval
         Position -> case getMemLoc m pval of
             Left ex -> Left $ InvalidLocation ("getParam:position " ++ show ex) pval
             Right v -> Right v
         Relative -> let relLoc = relBase m + pval
                      in case getMemLoc m relLoc of
                          Left ex -> Left $ InvalidLocation ("getParam:relative " ++ show ex) relLoc
                          Right v -> Right v


getParamM :: Members '[ Error MachineException
                      , State Machine
                      ] r
              => Instruction
              -> Sem r Int
getParamM ix = do
    m <- get @Machine
    let p1 = getParamIx 1 ix m -- Either
    either throw return p1


-- Execute 'op' on the two parameters and store it at the 3rd parameter in the
-- machine.  This is used for (+) (*) opcodes
doAction :: Members '[ State Machine
                     , CP.Log String
                     , Error MachineException
                     ] r
         => Instruction -> (Int -> Int -> Int)
         -> Sem r ()
doAction ix op = ((uncurry op <$> getTwoParamsM ix) >>= storeAtM ix 3) *> updateIP ix


jumpOp :: Members '[ State Machine
                   , CP.Log String
                   , Error MachineException
                   ] r
      => Instruction -> (Int -> Bool)
      -> Sem r ()
jumpOp ix test = do
    (p1, target) <- getTwoParamsM ix
    {-CP.log $ show ix ++ " test " ++ show p1 ++ " jumps " ++ show target-}
    if test p1 then modify @Machine (\m -> m { ip=target })
               else updateIP ix


inputOp :: Members '[ State Machine
                    , CP.Log String
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
        Right v -> storeAtM ix 1 v *> updateIP ix


-- try to get an input from the inList; if succesful, update the list and store
-- it in the right place, and return False.
-- Otherwise return True, which will cause the caller to exec the computation
-- so that another process can continue.
inputOpState :: Members '[ State Machine
                         , CP.Log String
                         , Error MachineException
                         ] r
             => Instruction
             -> Sem r Bool
inputOpState ix = do
    m <- get @Machine
    let vs = inList m
    if null vs
      then pure True  -- yield as we need input
      else do
        let (v:vs') = vs
        storeAtM ix 1 v
        updateIP ix
        modify @Machine (\m -> m { inList=vs' })
        pure False    -- don't yield as we had input


outputOp :: Members '[ State Machine
                     , Teletype
                     , Error MachineException
                     ] r
         => Instruction
         -> Sem r ()
outputOp ix = do
    p <- getParamM ix
    writeTTY $ show p
    updateIP ix


-- Output an Int to the outList of the machine.
outputOpState :: Members '[ State Machine
                          , Error MachineException
                          ] r
              => Instruction
              -> Sem r ()
outputOpState ix = do
    p <- getParamM ix
    modify @Machine (\m -> m { outList=outList m ++ [p] })
    updateIP ix


endOp :: Member (State Machine) r
      => Sem r ()
endOp = modify @Machine (\m -> m { ended=True })


adjustRelBaseOp :: Members '[ State Machine
                            , CP.Log String
                            , Error MachineException
                            ] r
                => Instruction
                -> Sem r ()
adjustRelBaseOp ix = do
    p <- getParamM ix
    modify @Machine (\m -> m { relBase=relBase m + p })
    m <- get @Machine
    CP.log $ "New relative base=" ++ show (relBase m)
    updateIP ix


updateIP :: Member (State Machine) r => Instruction -> Sem r ()
updateIP ix = modify @Machine (\m -> m { ip=ip m + size ix })


-- add an element to the input of a machine
appendToMachineInput :: Machine -> Int -> Machine
appendToMachineInput m v = m { inList=inList m ++ [v] }


peekMachineOutput :: Machine -> Maybe Int
peekMachineOutput m = case outList m of
    [] -> Nothing
    xs -> Just $ last xs


{-runWith :: Members '[ State Machine-}
                     {-, Teletype-}
                     {-, Error MachineException-}
                     {-] r-}
        {-=> [Int]-}
        {--> Sem r ()-}
        {--> IO (Either MachineException ())-}
-- I can't work out what type description to put in for 'runner' -- it ends up
-- with really hard types -- so we'll leave it up to polysemy.plugin and GHC to
-- infer the types
runWith opcodes runner =
    runner
        & evalState (loadMachine opcodes)      -- [Teletype, Log String, Embed IO, Error MachineException]
        & CP.runLogAction @IO logStringStdout  -- [Teletype, Embed IO, Error MachineException]
        & teletypeToIO                         -- [Embed IO, Error MachineException]
        & errorToIOFinal @MachineException     -- [Embed IO]
        & embedToFinal @IO
        & runFinal


{-
runWithState
  :: Polysemy.Internal.Sem
       '[State s, Colog.Polysemy.Effect.Log String, Teletype.Teletype,
         Polysemy.Error.Error MachineException,
         Polysemy.Embed.Type.Embed IO, Polysemy.Final.Final IO]
       a
     -> s -> IO (Either MachineException (s, a))
-}
runWithState runner machine =
    runner
        & runState machine                     -- [Teletype, Log String, Embed IO, Error MachineException]
        & CP.runLogAction @IO logStringStdout  -- [Teletype, Embed IO, Error MachineException]
        & teletypeToIO                         -- [Embed IO, Error MachineException]
        & errorToIOFinal @MachineException     -- [Embed IO]
        & embedToFinal @IO
        & runFinal


{-
runWithPure
  :: [Int]
     -> [String]
     -> Polysemy.Internal.Sem
          '[State Machine, Colog.Polysemy.Effect.Log o,
            Polysemy.Output.Output o, Teletype.Teletype,
            Polysemy.Error.Error e]
          a
     -> Either e ([String], a)
-}
runWithPure opcodes input runner =
    runner
        & evalState (loadMachine opcodes)      -- [Teletype, Log String, Error MachineException]
        & CP.runLogAsOutput                     -- [Teletype, Error MachineException]
        & ignoreOutput
        & runTeletypePure input                -- [Error MachineException]
        & runError                             -- []
        & run


-- this version is used when passing and returning the state.  This functions
-- type signature is:
{-runWithPureState
  :: [String]
     -> Polysemy.Internal.Sem
          '[State s, Colog.Polysemy.Effect.Log o, Polysemy.Output.Output o,
            Teletype.Teletype, Polysemy.Error.Error e]
          a
     -> s
     -> Either e ([String], (s, a))
-}
-- i.e. the State is returned in the Right part of the Either.
runWithPureState input runner machine =
    runner
        & runState machine                     -- [Teletype, Log String, Error MachineException]
        & CP.runLogAsOutput                     -- [Teletype, Error MachineException]
        & ignoreOutput  -- may want to push the output to a string?
        & runTeletypePure input                -- [Error MachineException]
        & runError                             -- []
        & run


-- These are the final opcode machine functions, as determined Day 13 (ish).

-- This is the main exec for the machine; it runs instructions until either the
-- input yields (this is the PURE version) or the program ends.
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


-- some helper functions to make setting up the machine a bit easier.

loadOpcodes ::  String -> IO [Int]
loadOpcodes opcodesFile = map (read . T.unpack) . T.split (==',')
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


-- Run the engine with an input until it yields or finishes.
runMachineWithInput :: [Int] -> Machine -> (Machine, [Int])
runMachineWithInput input m =
    let m' = m { inList=inList m ++ input }
        m'' = fixRunWithState m'
        os = outList m''
        m''' = m'' { outList=[] }
     in (m''', os)
