{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day17.M34
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.List         (break, foldl', intercalate, isPrefixOf,
                                    maximumBy, minimumBy, nub, permutations,
                                    reverse, unfoldr, (\\))

import           Data.Char         (chr, ord)
import           Data.Maybe        (fromMaybe, isJust, mapMaybe)

import           Control.Monad     (guard, void)
import           Safe              (headMay)

import           Lens.Micro        (each, ix, over, (%~), (&), (.~), (?~), (^.),
                                    (^..))
import           Lens.Micro.TH     (makeLenses)
import           Linear.V2         (V2 (..), _x, _y)

import qualified Data.HashMap.Lazy as H


-- OpCode Machine
import           OpCodeMachine     (Machine, MachineException, loadMachine,
                                    loadOpcodes, runMachineWithInput)

-- Drawing
import           Drawing           (Coord, Drawing, drawWith, getDimens)

--
-- now the test parts
opcodesFile = "files/17/opcodes.txt"


runCamera :: [Int] -> [Int]
runCamera opcodes =
    let (m, os) = runMachineWithInput [] (loadMachine opcodes)
     in os


runRobot :: [Int] -> [Int] -> [Int]
runRobot opcodes input =
    let (m, os) = runMachineWithInput input (loadMachine opcodes)
     in os


showImage :: [Int] -> String
showImage = map chr

-- load the image into a Image

type Image = H.HashMap Coord Int

-- this only is useful for the intersections.
decodeImage :: [Int] -> Image
decodeImage os = H.fromList $ fst $ foldl' go ([], V2 0 0) os
  where
      go :: ([(Coord, Int)], Coord) -> Int -> ([(Coord, Int)], Coord)
      go (xs, xy@(V2 x y)) i
        | i == 10 = (xs, V2 0 (y+1))
        | i == 46 = (xs, xy + V2 1 0)
        | otherwise = ((xy, i):xs, xy + V2 1 0)
        {-| i == 35 = ((xy, i):xs, xy + V2 1 0)-}
        {-| i `elem` robotOrds = ((xy,i):xs, xy + V2 1 0)-}
        {-| otherwise = (xs, xy + V2 1 0)-}


displayImage :: Image -> String
displayImage = drawWith chr 32

-- above, below, right and left
deltas :: [Coord]
deltas = [V2 (-1) 0, V2 1 0, V2 0 1, V2 0 (-1)]


-- find the intersections
itersections :: Image -> [Coord]
itersections im = map fst
                $ filter (isIntersection im)
                $ H.toList im


isIntersection :: Image -> (Coord, Int) -> Bool
isIntersection im (xy, v)
  | v /= 35 = False
  | otherwise = let adjacents = map (\d -> H.lookupDefault 0 (xy + d) im) deltas
                 in length (filter (==35) adjacents) == 4


calcResult :: [Coord] -> Int
calcResult cs = sum $ map (\(V2 x y) -> x * y) cs

-- part 2; work out the paths; manual inspection indicates that we just have to
-- run forwards until we find a corner.

windows :: Int -> [a] -> [[a]]
windows size xs
  | size > len || size < 0 = []
  | otherwise = [take size $ drop n xs | n <- [0 .. (len - size)]]
  where
    len = length xs

robotChrs :: String
robotChrs = "^>v<X"

robotOrds :: [Int]
robotOrds = map ord robotChrs

-- Manage the state of the Robot.
data Direction = North | East | West | South deriving Eq

instance Show Direction where
    show North = "N"
    show South = "S"
    show East  = "E"
    show West  = "W"


compass :: [Direction]
compass = [North, East, South, West]


robotOrdToDirection :: Int -> Maybe Direction
robotOrdToDirection r = lookup r (zip robotOrds compass)


deltaForDirection :: Direction -> V2 Int
deltaForDirection North = V2 0 (-1)
deltaForDirection South = V2 0  1
deltaForDirection East  = V2 1 0
deltaForDirection West  = V2 (-1) 0


turnDirection :: Direction -> Turn -> Direction
turnDirection North TurnLeft  = West
turnDirection North TurnRight = East
turnDirection South TurnLeft  = East
turnDirection South TurnRight = West
turnDirection East TurnLeft   = North
turnDirection East TurnRight  = South
turnDirection West TurnLeft   = South
turnDirection West TurnRight  = North

whichTurn :: Direction -> Direction -> Turn
whichTurn North East = TurnRight
whichTurn North West = TurnLeft
whichTurn South East = TurnLeft
whichTurn South West = TurnRight
whichTurn East North = TurnLeft
whichTurn East South = TurnRight
whichTurn West North = TurnRight
whichTurn West South = TurnLeft
whichTurn _ _        = error "Can't do reverse with Turns"

data Turn = TurnLeft | TurnRight deriving Eq

instance Show Turn where
    show TurnLeft  = "L"
    show TurnRight = "R"

-- analysis of drawing data
data Segment = Segment { _len :: Int, _thenTurn :: Maybe Turn } deriving Eq

instance Show Segment where
    show s = "<Segment " ++ show (_len s) ++ " steps, then " ++ show (_thenTurn s) ++ ">"


type Command = (Turn, Int)

showCommand :: Command -> String
showCommand (t, l) = "<Command: Turn " ++ show t ++ " and go " ++ show l ++ " steps>"


data ABC = A | B | C | None deriving Eq

instance Show ABC where
    show A    = "A"
    show B    = "B"
    show C    = "C"
    show None = "NotAssigned"


abcs = [A,B,C]

data Movement = Movement { _which    :: ABC
                         , _commands :: [Command]
                         } deriving Eq

makeMovement :: ABC -> [Command] -> Movement
makeMovement a cs = Movement {_which=a, _commands=cs}


instance Show Movement where
    show m = "<Movement " ++ show (_which m) ++ ": "
          ++ intercalate ", "(map show (_commands m)) ++ ">"


showSolver :: [Movement] -> String
showSolver = intercalate ", " . map show


-- Segments and Commands
-- Segments are the analysis, whereas commands are the result.
-- These then go into the Solver.


commandsToMovement :: [Command] -> Movement
commandsToMovement = makeMovement None


isWindowInMovements :: [Command] -> [Movement] -> Bool
isWindowInMovements cs ms =
    let ms' = filter ((==None)._which) ms
        finder x = isJust $ findSequenceIndex cs (_commands x)
     in any finder ms'


windowsFromMovement :: Movement -> [[Command]]
windowsFromMovement m =
    let cs = _commands m
        l = length cs
     in cs : concatMap (`windows` cs) [l-1,l-2..2]


newMovement :: [Movement] -> [Command] -> Maybe [Movement]
newMovement ms cs =
    let ms' = nub $ map _which ms
     in if length ms' == 4
          then Nothing
          else let nextABC = head $ [A,B,C,None] \\ ms'
                   movement = makeMovement nextABC cs
                in Just $ filter (not.null._commands)
                        $ insertSequence movement ms


tryWindowsInMovements :: [Movement] -> Maybe [[Movement]]
tryWindowsInMovements ms = do
    unsolved <- headMay $ filter ((==None)._which) ms
    let tryWindows = windowsFromMovement unsolved
        validTries = concat $ mapMaybe (tryWindowInMovements' ms) tryWindows
    guard $ not (null validTries)
    pure validTries


tryWindowInMovements' :: [Movement] -> [Command] -> Maybe [[Movement]]
tryWindowInMovements' ms cs = do
    ms' <- newMovement ms cs
    guard $ areMovementValidLengths ms'
    if isSolved ms'
      then pure [ms']
      else tryWindowsInMovements ms'


-- Note only checks the assigned movements
areMovementValidLengths :: [Movement] -> Bool
areMovementValidLengths ms =
    let ms' = filter ((/=None)._which) ms
        ls = map (length.movementToString) ms'
        l = length $ movementsToRoutineString ms'
     in all (<=20) ls  && (l <= 20)


movementToString :: Movement -> String
movementToString m = intercalate "," (map stringify (_commands m))
  where
      stringify (t,l) = show t ++ "," ++ show l


movementsToRoutineString :: [Movement] -> String
movementsToRoutineString ms = intercalate "," (map (show._which) ms)

solutionStrings :: [Movement] -> [String]
solutionStrings ms =
    let routine = movementsToRoutineString ms
        fnA = head $ filter ((==A)._which) ms
        fnB = head $ filter ((==B)._which) ms
        fnC = head $ filter ((==C)._which) ms
        moves = map movementToString [fnA, fnB, fnC]
     in routine : moves


insertSequence :: Movement -> [Movement] -> [Movement]
insertSequence m = insertSequence' m []


insertSequence' :: Movement -> [Movement] -> [Movement] -> [Movement]
insertSequence' _ ds [] = ds
insertSequence' m ds (m':ms) =
    case removeSubSequence (_commands m) (_commands m') of
        Nothing -> insertSequence' m (ds ++ [m']) ms
        Just (bs, as) ->
            insertSequence' m
                            (ds ++ [makeMovement None bs, m])
                            (makeMovement None as : ms)

removeSubSequence :: Eq a => [a] -> [a] -> Maybe ([a],[a])
removeSubSequence as bs =
    findSequenceIndex as bs >>= \i ->
        pure (take i bs, drop (i + length as) bs)


findSequenceIndex :: Eq a => [a] -> [a] -> Maybe Int
findSequenceIndex as bs = findSequenceIndex' as bs 0


findSequenceIndex' :: Eq a => [a] -> [a] -> Int -> Maybe Int
findSequenceIndex' as bs i
  | null bs || null as = Nothing
  | length as > length bs = Nothing
  | otherwise = if as `isPrefixOf` bs
                  then Just i
                  else findSequenceIndex' as (tail bs) (i+1)



isSolved :: [Movement] -> Bool
isSolved ms =
    let remain = length $ filter (==None) (map _which ms)
     in remain == 0


findSegments :: Drawing -> Coord -> Direction -> ([Segment], Coord, Direction)
findSegments dr xy d = findSegments' dr xy d []

findSegments' :: Drawing -> Coord -> Direction -> [Segment] -> ([Segment], Coord, Direction)
findSegments' dr xy d ss =
    let (segment, xy') = findCornerOrEnd dr xy d
     in case _thenTurn segment of
         Nothing -> (reverse (segment:ss), xy, d)
         Just turn -> findSegments' dr xy' (turnDirection d turn) (segment:ss)


segmentToCommand :: Turn -> Segment -> (Command, Maybe Turn)
segmentToCommand t s = ((t, _len s), _thenTurn s)


segmentsToCommands :: Turn -> [Segment] -> [Command]
segmentsToCommands t ss = segmentsToCommands' t ss []

segmentsToCommands' :: Turn -> [Segment] -> [Command] -> [Command]
segmentsToCommands' _ [] cs = reverse cs
segmentsToCommands' t (s:ss) cs =
    let (c, nextT) = segmentToCommand t s
     in case nextT of
         -- end it at Nothing, regardless of whether there are more segments.
         Nothing     -> segmentsToCommands' t [] (c:cs)
         Just nextT' -> segmentsToCommands' nextT' ss (c:cs)


-- find corner
findCornerOrEnd :: Drawing -> Coord -> Direction -> (Segment, Coord)
findCornerOrEnd dr xy d = findCornerOrEnd' dr xy d 0


findCornerOrEnd' :: Drawing -> Coord -> Direction -> Int -> (Segment, Coord)
findCornerOrEnd' dr xy d n =
    let adjacents = around dr xy d
        ended = isEnd adjacents
        mustTurn = isCorner adjacents
     in case (ended, mustTurn) of
         (True, _) -> (Segment {_len=n, _thenTurn=Nothing}, xy)
         (False, d'@(Just _)) -> (Segment {_len=n, _thenTurn=d'}, xy)
         _ -> findCornerOrEnd' dr (xy + deltaForDirection d) d (n+1)



-- return the ins at [ahead, left, right]
around :: Drawing -> Coord -> Direction -> [Int]
around dr xy d =
    let deltaAhead = deltaForDirection d
        deltaLeft = deltaForDirection (turnDirection d TurnLeft)
        deltaRight = deltaForDirection (turnDirection d TurnRight)
     in map (lookupDelta dr xy) [deltaAhead, deltaLeft, deltaRight]


isCorner :: [Int] -> Maybe Turn
isCorner [forward, left, right] =
     case (forward, left, right) of
         (0, 35, 0) -> Just TurnLeft
         (0, 0, 35) -> Just TurnRight
         _          -> Nothing

-- if it's the end the ahead, left, right are all 0 .. i.e. nowhere to go
isEnd :: [Int] -> Bool
isEnd xs = sum xs == 0

lookupDelta :: Drawing -> Coord -> Coord -> Int
lookupDelta dr xy dxy = H.lookupDefault 0 (xy + dxy) dr


findStartDirection :: Drawing -> Coord -> Direction
findStartDirection dr xy =
    case map (lookupDelta dr xy . deltaForDirection) compass of
        [35, 0, 0, 0] -> North
        [0, 35, 0, 0] -> East
        [0, 0, 35, 0] -> South
        [0, 0, 0, 35] -> West
        _             -> error "Couldn't find the direction????"


findRobot :: Drawing -> (Coord, Maybe Direction)
findRobot dr =
    let (xy, i) = head $ filter ((`elem` robotOrds).snd) $ H.toList dr
     in (xy, robotOrdToDirection i)


-- finally convert the output to a set of strings that we'll then covert
-- to an int and get the output
findSolutions :: [Int] -> Maybe [[String]]
findSolutions s =
    let dr = decodeImage s
        c@(xy, Just d) = findRobot dr
        d' = findStartDirection dr xy
        (segments, xy', finalD) = findSegments dr xy d'
        commands = segmentsToCommands (whichTurn d d') segments
        movement = commandsToMovement commands
        tries = tryWindowsInMovements [movement]
        solutions = map solutionStrings <$> tries
     in solutions


solutionToInput :: String -> [Int]
solutionToInput = map ord


runRobotWithSolution :: [Int] -> [String] -> [Int]
runRobotWithSolution ops soln =
    -- switch robot on
    let ops' = 2 : tail ops
        video = "n"
        cmd = unlines $ soln ++ [video]
     in runRobot ops' (solutionToInput cmd)


main34 :: IO ()
main34 = do
    putStrLn "AOC2019 Day 17 Part 2 - Set and Forget - scaffolding fun"
    opcodes <- loadOpcodes opcodesFile
    let s = runCamera opcodes
    let im = decodeImage s
    putStrLn $ displayImage im
    print (itersections im)
    putStrLn $ "The result is " ++ show (calcResult (itersections im))
    let c@(xy, Just d) = findRobot im
    putStrLn $ "Robot is at " ++ show c
    let d' = findStartDirection im xy
    putStrLn $ "Start direction is " ++ show d'
    let solution = head $ fromMaybe [] $ findSolutions s
        res = runRobotWithSolution opcodes solution
    putStrLn $ "Trying with:\n" ++ show (unlines solution)
    let im' = decodeImage $ take (length res -1) res
    putStrLn $ displayImage im'
    putStrLn $ "The following output: " ++ show (last res)
