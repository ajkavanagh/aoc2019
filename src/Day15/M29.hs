{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day15.M29
    {-( -}
    {-)-}
      where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Data.List         (foldl', intercalate, maximumBy, minimumBy,
                                    permutations, reverse)

import           Control.Monad     (void)
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
import           Drawing           (Coord, drawWith, getDimens)


{-
  How to search the space; we'll do it brute force to start with, and only
  switch to A* if the performance of brute force is too slow.

  The 'state' is held in the opcode machine, but we need to generate a copy of
  the map.  Also, we have to record where we've been, and what options there
  were at each location.  And we can only do this by moving.

  So we start at (0,0) and have four options: NSEW
  Then we move North. Now we have  "locations, unexplored directions, " of
    (0,0) SEW
    (0,-1) NEW N

  If we go North again we will have:
    (0,0) SEW
    (0,-1) EW N
    (0,-2) NEW NN

  (note that 'N' has been removed from (0,-1) PathItem because we choose to move
  North

  If we went West at this point then we'd have:
    (0,0) SEW
    (0,-1) EW N
    (0,-2) EW NN
    (-1,-2) NWS NNW

  We also wish to record successful routes.  We can store that by just storing
  the directions.

  So we have PathItem strutures which contain coord, unexplored, path

   If (-1,-2) is a dead end, then eventually we'll have

   (0,0) SEW
   (0,-1) EW N
   (0,-2) EW NN
   (-1,-2) - NNW

   At this point the NEXT move is to back up the oposite of 'W" which is 'E':
   ...
   (0,-2) EW NN

  So next we choose E:

    ...
    (0,-2) W NN
    (1,-2) NEW NNE

  So for each move choice we have to update the current PathItem and then build
  the new one.  Essentially, we try to visit each location.
  Technically, we don't need to store each location in the Path item; just the
  options at each location and where we've been to get there.
-}


-- now the test parts
opcodesFile = "files/15/opcodes.txt"


-- Manage the state of the Robot.
data Direction = North | East | West | South deriving Eq

instance Show Direction where
    show North = "N"
    show South = "S"
    show East  = "E"
    show West  = "W"

directionToInt :: Direction -> Int
directionToInt North = 1
directionToInt East  = 4
directionToInt West  = 3
directionToInt South = 2

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection South = North
reverseDirection East  = West
reverseDirection West  = East

intToDirection :: Int -> Direction
intToDirection 1 = North
intToDirection 2 = South
intToDirection 3 = West
intToDirection 4 = East
intToDirection x = error $ "Can't convert direction " ++ show x ++ "!!"

deltaForDirection :: Direction -> V2 Int
deltaForDirection North = V2 0 (-1)
deltaForDirection South = V2 0  1
deltaForDirection East  = V2 1 0
deltaForDirection West  = V2 (-1) 0


type PathItems = H.HashMap Coord [Direction]


allDirections :: [Direction]
allDirections = [North, East, West, South]

emptyPathItems :: PathItems
emptyPathItems = H.empty


data Robot = Robot { _robotLocation    :: Coord
                   , _robotCoords      :: H.HashMap Coord Int
                   , _robotPathItems   :: PathItems
                   , _robotCurrentPath :: [Direction]
                   , _robotPathsFound  :: [[Direction]]
                   , _robotRetracing   :: Bool
                   , _robotOxygenLoc   :: Maybe Coord
                   , _robotMachine     :: Machine
                   }

makeLenses ''Robot


instance Show Robot where
    show r = "Location: " ++ show (_robotLocation r)
          ++ "Options: " ++ show (H.lookupDefault allDirections (r ^. robotLocation) (r ^. robotPathItems))
          ++ "\nCurrent Path: " ++ intercalate "" (map show (reverse (r ^. robotCurrentPath)))
          ++ "\nPaths Found: " ++ intercalate ", " (map show (_robotPathsFound r))
          ++ "\nOxygen: " ++ (case _robotOxygenLoc r of
              Nothing -> "Not found yet!"
              Just xy -> show xy)
          ++ "\nBacktracking: " ++ (if _robotRetracing r then "Yes" else "No")
          {-++ ", machine not shown."-}
          ++ "\n"
          ++ drawWith day15toChar (-1) (_robotCoords r)


day15toChar :: Int -> Char
day15toChar 0 = '.'
day15toChar 1 = '#'
day15toChar 2 = 'D'
day15toChar 3 = 'O'
day15toChar _ = ' '


newRobot :: [Int] -> Robot
newRobot os = Robot { _robotLocation=V2 0 0
                    , _robotCoords = H.empty
                    , _robotPathItems = emptyPathItems
                    , _robotCurrentPath = []
                    , _robotPathsFound = []
                    , _robotRetracing = False
                    , _robotOxygenLoc = Nothing
                    , _robotMachine=loadMachine os
                    }

runRobotWithInput :: Direction -> Robot -> (Int, Robot)
runRobotWithInput d r =
    let (m, o:os) = runMachineWithInput [directionToInt d] (r ^. robotMachine)
        r' = r & robotMachine .~ m
     in (o, r')


-- Note that the program nevers ends.  We have to decide when to end based on
-- the state we've generated.
runEngineUpdateRobot :: Direction -> Robot -> Robot
runEngineUpdateRobot dir r =
    let (o, r') = runRobotWithInput dir r
        (r'', move) = updateRobotAndChooseMove dir o r'
     in case move of
         Nothing  -> r''
         Just dir -> runEngineUpdateRobot dir r''


runEngineUpdateRobotIO :: Direction -> Robot -> IO Robot
runEngineUpdateRobotIO dir r = do
    let (o, r') = runRobotWithInput dir r
        (r'', move) = updateRobotAndChooseMove dir o r'
    putStrLn $ "Sent " ++ show dir ++ ", received: " ++ show o ++ ", choose " ++ show move
    print r''
    getLine
    case move of
        Nothing  -> pure r''
        Just dir -> runEngineUpdateRobotIO dir r''


updateRobotAndChooseMove :: Direction
                         -> Int  -- the character for the cell to fill in
                         -> Robot
                         -> (Robot, Maybe Direction)
updateRobotAndChooseMove d status r
  | status >=0 && status <= 2 =
    let delta  = deltaForDirection d
        hitWall = status == 0
        loc    = r ^. robotLocation
        newLoc = loc + delta
        robotLoc = (if hitWall then loc else newLoc)
        backtracking = r ^. robotRetracing
        coordInt = case status of
            0 -> 1
            1 -> 2
            2 -> 3
        existing = H.lookupDefault 0 loc (r ^. robotCoords)
        clearIt = (if existing /= 3 then 0 else 3)
        coordUpdate = (if hitWall
                        {-then H.insert newLoc 1-}
                        then H.union (H.fromList [(loc, existing), (newLoc, coordInt)])
                        else H.union (H.fromList [(loc, clearIt), (newLoc, coordInt)]))
        currentPathUpdate
          | hitWall = id
          | backtracking = tail
          | otherwise = (d :)
        pathsFoundUpdate = (if status == 2
                              then ((d : r ^. robotCurrentPath) :)
                              else id)
        oxygenUpdate = (if status == 2
                          then const $ Just newLoc
                          else id)
        r' = r & robotCoords %~ coordUpdate
               & robotLocation .~ robotLoc
               & robotPathItems %~ goDirection d loc
               & robotCurrentPath %~ currentPathUpdate
               & robotPathsFound %~ pathsFoundUpdate
               & robotOxygenLoc %~ oxygenUpdate
     in chooseRobotMove r'
  | otherwise = error $ "Status " ++ show status ++ " received; can't handle that"


goDirection :: Direction -> Coord -> PathItems -> PathItems
goDirection d xy pi = let p = H.lookupDefault allDirections xy pi
                          p' = filter (/=d) p
                          newXY = xy + deltaForDirection d
                          nextPi = H.lookupDefault allDirections newXY pi
                          reverseD = reverseDirection d
                          nextPi' = filter (/=reverseD) nextPi
                       in H.union (H.fromList [(xy, p'), (newXY, nextPi')]) pi

--
-- choose the next possible move, based on the robot location and available
-- moves.
chooseRobotMove :: Robot -> (Robot, Maybe Direction)
chooseRobotMove r =
    let pi = r ^. robotPathItems
        xy = r ^. robotLocation
        path = r ^. robotCurrentPath
        move = headMay =<< H.lookup xy pi
     in case move of
            Just _ -> (r & robotRetracing .~ False, move)
            Nothing -> (r & robotRetracing .~ True, reverseDirection <$> headMay path)


-- test functions
testIt :: IO Robot
testIt = do
    opcodes <- loadOpcodes opcodesFile
    let r = newRobot opcodes
        r' = runEngineUpdateRobot North r
    pure r'


main29 :: IO ()
main29 = do
    putStrLn "AOC2019 Day 15 Part 1 - Oxygen System"
    opcodes <- loadOpcodes opcodesFile
    let r = newRobot opcodes
    {-r' <- runEngineUpdateRobotIO North r-}
        r'= runEngineUpdateRobot North r
    print r'
    putStrLn $ "\nLengths of paths: " ++ intercalate ", " (map (show.length) (r' ^. robotPathsFound))
