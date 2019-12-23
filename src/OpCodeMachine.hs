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


import           Data.Either       (either, fromLeft, fromRight, isLeft, lefts, rights)
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
                                    runFinal, run)
import           Polysemy.Error    (Error, errorToIOFinal, throw, runError)
import           Polysemy.State    (State, evalState, get, modify, put, runState)
import           Polysemy.Output   (ignoreOutput)

-- for safe reading
import           Text.Read         (readEither)


-- Effect for input & output
import           Teletype          (Teletype, readTTY, teletypeToIO, writeTTY, runTeletypePure)


type Memory = Array Int Int


data Machine = Machine { memory  :: Memory
                       , ip      :: Int
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
        deriving (Show, Eq)


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
                               , params   :: [Int]
                               , location :: Int
                               }

instance Show Instruction where
    {-show i = "Instruction<" ++ show (opCode i)-}
          {-++ ", size=" ++ show (size i)-}
          {-++ ", modes=" ++ show (modes i)-}
          {-++ ", at location=" ++ show (location i)-}
          {-++ ">"-}
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


showParam :: Instruction -> Int -> String
showParam ix i =
    let _mode = modes ix !! i
        v     = params ix !! i
    in if _mode == Immediate
         then show v
         else "(" ++ show v ++ ")"


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
loadMachine xs = Machine { memory=listArray (0, length xs -1) xs
                         , ip=0
                         , ended=False
                         , inList=[]
                         , outList=[]
                         }


storeAt :: Machine -> Int -> Int -> Machine
storeAt m i v = m { memory=mem', ip=ip m }
  where
      mem' = runST $ do
        arr <- thaw (memory m) :: ST s (STArray s Int Int)
        writeArray arr i v
        freeze arr


storeAtM :: Members '[ State Machine
                     , Error MachineException
                     ] r
         => Instruction
         -> Int -- parameter number
         -> Int -- value
         -> Sem r ()
storeAtM ix p v = do
    m <- get @Machine
    -- parameter mode is on param p and must be Position
    let mode = modes ix !! (p -1)
        i = ip m + p
        mem = memory m
        (s,e) = bounds mem
    if mode /= Position
      then throw $ InvalidParameterMode mode p i
      else if i < s || e > e
        then throw $ InvalidLocation "storeAtM" i
        else let loc = mem ! i in
            if loc < s || loc > e
              then throw $ InvalidLocation "storeAtM 2nd part" loc
              else put @Machine $ storeAt m loc v


decodeInstructionUsing :: (Int -> Maybe Op)    -- int to opcode
                       -> (Int -> Maybe Mode)  -- int to the parameter mode
                       -> (Op -> Int)          -- Opcode to size of instruction
                       -> Machine              -- Machine to use.
                       -> Either MachineException Instruction
decodeInstructionUsing iToOpCode iToParamMode opToInt m =
    let i = ip m
        mem = memory m
        (s,e) = bounds mem
        code = mem ! i
        maybeOp = iToOpCode $ code `mod` 100
        modeP1 = (code `mod` 1000) `div` 100
        modeP2 = (code `mod` 10000) `div` 1000
        modeP3 = (code `mod` 100000) `div` 10000
    in if i < s || i > e
         then Left $ InvalidInstructionPointer i
         else
            let op = maybe (Left $ InvalidOpCode code i) Right maybeOp
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
getMemLoc m p =
    let mem = memory m
        (s,e) = bounds mem
    in if p < s || p > e
         then Left $ InvalidLocation "getMemLoc" p
         else Right $ mem ! p


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


getParam :: Mode -> Int -> Memory -> Either MachineException Int
getParam mode pos mem =
    let (s,e) = bounds mem
        p1 = mem ! pos
     in if pos < s || pos > e
      then Left $ InvalidLocation "getParam" pos
      else let imm = mem ! pos
            in case mode of
                Position -> if imm < s || imm > e
                              then Left $ InvalidLocation "getParam:position" imm
                              else Right $ mem ! imm
                Immediate -> Right imm


getParamM :: Members '[ Error MachineException
                      , State Machine
                      ] r
              => Instruction
              -> Sem r Int
getParamM ix = do
    m <- get @Machine
    let p1 = getParam (head (modes ix)) (ip m +1) (memory m)  -- Either
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
