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

module Day07.M14
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

import           Data.List         (intercalate, foldl', permutations, maximumBy)

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
                                    Mode (..), Op (..), decodeInstructionUsing,
                                    doAction, inputOp, opCode, outputOp, jumpOp,
                                    runWith, runWithPure, runWithState, ip, memory, ended, inList, outList, inputOpState, outputOpState, loadMachine, appendToMachineInput, peekMachineOutput, endOp)

{-
   Day 7: Part 2, feed back loops for the amlifier

   How on earth is this going to work?

      O-------O  O-------O  O-------O  O-------O  O-------O
0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
   |  O-------O  O-------O  O-------O  O-------O  O-------O |
   |                                                        |
   '--------------------------------------------------------+
                                                            |
                                                            v
                                                     (to thrusters)

So the issue is: each amp has to have a phase, and then a series of signals until
it ends.  Thus the Machine type probably needs to be extended to include:

- the status of the machine (running/done)
- an input and output stream.

If these are encoded in the state, then we can
link them to each other, and keep running machines until they are all completed.

I'm guessing the machine E has to hit the '99' End opcode, to complete.

So, we'll extend the Machine to include the input/output (as lists) and then
ensure we can get the Machine out of the runPure function.

This is done in the OpCodeMachine module ^^^.
-}

import Day05.M10 (decodeInstruction)

-- we need to change the M10 version of exec to use the inputOpState and
-- outputOpState and to get the Machine back out of the runtime.  This means
-- returning from this function if the program requires input and there is
-- nothing in the Machine inList, or if the program has ended
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
  where
      toBool True = 1
      toBool False = 0
      lt x y = toBool $ x < y
      eq x y = toBool $ x == y


-- Now we need to hold n machines, and connect the output to the next input and
-- back to the first one, and run them all until the final machine ends.  Read
-- the last output and that's the result.  We keep all the outputs (just
-- because) and just use the last one.  We use up the inputs as that's when a
-- machine has to 'wait' for the next input (which is five machine runs later).


-- let's hold n machines.
type Amplifier = Array Int Machine


-- initialise a Machine with opcodes and a phase (according to the local
-- funcion)
initialiseMachineWith :: [Int] -> Int -> Machine
initialiseMachineWith opcodes phase =
    let m = loadMachine opcodes in m { inList=[phase] }


-- initialiseAmplifier  .. from 5 ints (the phases) and the opcodes.
initialiseAmplifier :: [Int] -> [Int] -> Amplifier
initialiseAmplifier opcodes phases =
    listArray (0, length phases -1) $ map (initialiseMachineWith opcodes) phases


-- run an a machine identified with N in the amplifier with an input, and
-- produce the output.  The machine runs either till it ends or asks for a new
-- input.  The last item on the output is peeked and provide.
runMachineNInAmplifierWith :: Monad m
                           -- runner function for the Machine, input
                           => (Machine -> m Machine)
                           -> Amplifier
                           -> Int
                           -> Int
                           -> m (Int, Amplifier)
runMachineNInAmplifierWith f amp n input = do
    let m = amp ! n
        m' = appendToMachineInput m input
    m'' <- f m'
    case peekMachineOutput m'' of
        Just output -> pure (output, updateMachineInAmplifier amp n m'')
        -- this is horrible; TODO we'll fix this by passing a constructor for
        -- the error.
        Nothing -> fail "Something went wrong"


fixRunWithStateIO :: Machine -> IO Machine
fixRunWithStateIO m = do
    res  <- runWithState exec m :: IO (Either MachineException (Machine, ()))
    case res of
        Right (m', _) -> pure m'
        -- this is also horrible; TODO work out how to fix this so we don't have
        -- to use fail
        Left ex -> fail $ show ex


-- This runner, runs a single machine with input:
runMachineNInAmplifier :: Amplifier -> Int -> Int -> IO (Int, Amplifier)
runMachineNInAmplifier = runMachineNInAmplifierWith fixRunWithStateIO

-- now run all the machines for a single cycle and return the output
runAmplifierOnce :: Amplifier -> Int -> IO (Int, Amplifier)
runAmplifierOnce amp input = foldM go (input, amp) [s..e]
  where
      (s, e) = bounds amp
      go :: (Int, Amplifier) -> Int -> IO (Int, Amplifier)
      go (n, amp') i = runMachineNInAmplifier amp' i n


-- finally, keep running the machines until we get the final machine has eneded.
runAmplifier :: Amplifier -> Int -> IO Int
runAmplifier amp input = do
    (out, amp') <- runAmplifierOnce amp input
    let (_, e) = bounds amp'
        m = amp' ! e
        done = ended m
    if not done
      then runAmplifier amp' out
      else pure out



-- Update a single machine
updateMachineInAmplifier :: Amplifier -> Int -> Machine -> Amplifier
updateMachineInAmplifier ms n m = runST $ do
    arr <- thaw ms :: ST s (STArray s Int Machine)
    writeArray arr n m
    freeze arr


opcodesFile = "files/07/opcodes.txt"


loadOpcodes ::  IO [Int]
loadOpcodes = map (read . T.unpack) . T.split (==',')
          <$> TIO.readFile opcodesFile

-- Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5)
loadTest1 :: [Int]
loadTest1 =
    let block = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,"
             <> "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
     in map (read . T.unpack) . T.split (==',') $ block

-- Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6)
loadTest2 :: [Int]
loadTest2 =
    let block = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,"
             <> "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,"
             <> "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
     in map (read . T.unpack) . T.split (==',') $ block


runPure :: [Int] -> [String] -> Either MachineException ([String], ())
runPure opcodes input = runWithPure opcodes input exec


runIO :: [Int] -> IO (Either MachineException ())
runIO opcodes = runWith opcodes exec


-- // HERE - replace following
--
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


findHighest2 :: IO String
findHighest2 = do
    let perms = permutations [5,6,7,8,9]
    opcodes <- loadOpcodes
    let go :: ([Int] -> IO (String, Int))
        go phases = do
            out <- runAmplifier (initialiseAmplifier opcodes phases) 0
            pure (concatMap show phases, out)
    combs <- mapM go perms
    let max_comb = maximumBy (\x y -> snd x `compare` snd y) combs
    pure $ fst max_comb ++ " gives " ++ show (snd max_comb)

-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

main14 :: IO ()
main14 = do
    putStrLn "AOC2019 Day 7 Part 2"
    res <- findHighest2
    print res

