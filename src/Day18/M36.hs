{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}

module Day18.M36 where

import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Composition           ((.:))

{-import           Data.Array      (Array, bounds, elems, listArray, (!))-}
import           Control.Monad              (forM, forM_, guard, mapM_, when, foldM, unless, filterM)
import           Control.Monad.Primitive    (PrimState)
import           Control.Monad.ST           (ST, runST)
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.RWS.Strict   as RWS
import           Control.Monad.RWS.Strict   (ask, tell, gets, modify')

import           Data.Array                 (Array)
import qualified Data.Array                 as DA
import           Data.Array.MArray          (freeze, newListArray, readArray,
                                             thaw, writeArray)
import           Data.Array.ST              (STArray, newListArray)
import           Data.Array.IO              (IOArray)
import qualified Data.Array.IO              as DAIO

import           Data.Word                  (Word64)
import           Data.Function              (on)
import qualified Data.Bits                  as DB

import qualified Data.Char                  as C
import           Data.List                  (groupBy, intercalate, isInfixOf,
                                             isPrefixOf, nub, nubBy, sort,
                                             sortBy, sortOn, (\\), partition, unfoldr)
import           Data.List.Split            (chunksOf, keepDelimsL, split,
                                             whenElt)
import qualified Data.DList                 as DL
import qualified Data.Maybe                 as M

import           Data.Hashable              (Hashable (..))
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as HS

{-import           Data.HashPSQ               (HashPSQ)-}
import qualified Data.HashPSQ               as QH
--import qualified Data.IntPSQ                as Q
import qualified Data.OrdPSQ                as Q

import qualified Data.Map                   as DM
import           Data.Semigroup
import           FloydWarshall              (findMinDistances,
                                             showShortestPaths)
import qualified FloydWarshall              as FW
import           Utils                      (splits, insertIntoList)

import           Lens.Micro                 (both, each, ix, over, (%~), (&),
                                             (.~), (?~), (^.), _1, _2)

import           Text.Printf                (printf)
-- debuging
import           Debug.Trace                (trace)


mazeFile = "files/18/maze.txt"


loadMaze :: IO T.Text
loadMaze = TIO.readFile mazeFile

-- Analysis phase; load the maze into vectors of Char

type Coord = (Int,Int)
type Maze = Array Coord Char

readMap :: T.Text -> Maze
readMap txt =
    let ls = T.lines txt
        ydim = length ls
        xdim = T.length (head ls)  -- assume they are all the same length
     in DA.listArray ((0,0),(ydim-1,xdim-1)) $ T.unpack $ T.concat ls


-- for part 2, adjust the maze into 4 parts
adjustMaze :: Maze -> Maze
adjustMaze maze =
    let xy = findEntrance maze
        wall = [(a,b) | a <- [-1..1], b <- [-1..1], a /= b]
        corners = [(a,b) | a <- [-1..1], b <- [-1..1], (a == b || a == (-b)) && a /=0]
        adjust = map (\(xy', c) -> (bothsum xy xy', c)) (zip wall (repeat '#') ++ [((0,0), '#')] ++ zip corners (repeat '@'))
     in maze DA.// adjust


mazeToString :: Maze -> [String]
mazeToString maze =
    let ((_,ymin),(_,ymax)) = DA.bounds maze
     in chunksOf (ymax - ymin +1) $ DA.elems maze


-- Let's have a data item.  it's a path, the number of keys found, and the cost
-- to traverse that path.
-- Robot tracker that tracks the places of the 4 robots for a keyCode (a
-- collection of keys)
-- The _cachedCosts is a mapping of xy -> cost, where all four robots have to
-- have a better cost at that location for that key before being (better) or
-- (worse)

type CachedCosts2 = H.HashMap (Coord, Coord, Coord, Coord) Int

data RTracker = RTracker { _keyCode :: !KeyCode
                         , _cachedCosts :: !CachedCosts2
                         {-, _traces :: ![RTrace]-}
                         , _tracesQueue :: !(TraceQueue)
                         }

instance Show RTracker where
    {-show RTracker {_keyCode=key, _traces=traces, _cachedCosts=costs}-}
    show RTracker {_keyCode=key, _tracesQueue=tq, _cachedCosts=costs}
      = "\nRT(code=" ++ show key
      {-++ ", num-traces: " ++ show (length traces)-}
      ++ ", num-traces: " ++ show (QH.size tq)
      ++ ", traces:[\n" ++ intercalate "\n" (map show $ take 4 (QH.toList tq))
      ++ "\n cachedCosts= " ++ intercalate ", " (map show (H.toList costs))
      ++ "])"


type KeyCode = Word64


-- the cost is ONLY the keys; i.e. we don't record backtracking by a robot for a
-- Door.
robotToCost :: Robot -> Cost
robotToCost Robot{_robotCost=cost, _robotPath=p} = Cost (length fp, cost)
  where
      fp = filter C.isLower p


-- a cost element = (path length, Cost): used to stop Robots backtracking where
-- they came from unless they have more keys/doors.  The first part of the pair
-- is the number of keys that robot has, the second is the current cost at that
-- point in it's life
newtype Cost = Cost (Int, Int) deriving (Show, Eq)

instance Ord Cost where
    compare (Cost (k1, c1)) (Cost (k2, c2)) = case compare k2 k1 of
        EQ -> compare c1 c2
        x -> x

instance Hashable Cost where
    hashWithSalt i (Cost (k, c)) = i + k + c


data Robot = Robot { _robotPath :: !String   -- full path by this robot
                   , _robotCost :: !Int      -- cost of the path (number of steps)
                   , _robotXY :: !Coord      -- where it is
                   , _robotBlocked :: !Bool  -- whether it can move (i.e. it might be blocked by a Door)
                   , _robotCode :: !KeyCode  -- the partial code stored in this robot
                   , _robotHistory :: !(H.HashMap Coord Cost)  -- where it has been (with costs)
                   }


makeNewRobot :: Coord -> Robot
makeNewRobot xy = Robot { _robotPath=""
                        , _robotCost=0
                        , _robotXY=xy
                        , _robotBlocked=False
                        , _robotCode=0x00
                        , _robotHistory=H.empty
                        }


instance Show Robot where
    show r@Robot{ _robotPath=p
                , _robotCost=c
                , _robotXY=xy
                , _robotBlocked=b
                , _robotCode=kc
                , _robotHistory=h
                }
        =  "R[" ++ show p ++ ", cost=" ++ show c ++ ", at:" ++ show xy
        ++ ", blocked=" ++ show b ++ ", history-len=" ++ show (H.size h)
        ++ ", partial code=" ++ printf "0x%08x" kc
        ++ ", pathCost here=" ++ show (robotToCost r)
        ++ ", history=" ++ intercalate ", " (map show (H.toList h))
        ++ "]"

-- two robots have the same hash if they have the same hash if they are at the
-- same place with the same keypath.  i.e. we consider them the same if they are
-- in the same place.  If they have a worse cost, then they'll be elimiated.
instance Hashable Robot where
    hashWithSalt i Robot{_robotPath=rp, _robotCost=rc, _robotXY=xy,
                         _robotBlocked=b, _robotCode=kc, _robotHistory=h} =
        i + hash xy + fromIntegral kc


-- two robots are equal if their code and xy locations are the same.  They are
-- not 'equal' based on their path or history; this is how we elimiate worse
-- robots.
instance Eq Robot where
    Robot{_robotCode=kc1, _robotXY=xy1} == Robot{_robotCode=kc2, _robotXY=xy2} =
        kc1 == kc2 && xy1 == xy2


data RTrace = RTrace { _tKeyCode :: !KeyCode -- the combined path keycode
                     , _tCost :: !Int   -- the combined cost of all Robots
                     , _tSumPath :: !Int -- the combined sum of all path lengths
                     , _tHashForEquals :: !Int -- the hash of the value for Equals
                     , _tRobots :: ![Robot]
                     }


instance Show RTrace where
    show RTrace{_tKeyCode=code, _tCost=c, _tRobots=rs}
      =  "Trace{" ++ printf "0x%08x" code ++ ", cost=" ++ show c
      ++ ", robots=\n" ++ intercalate "\n" (map show rs)
      ++ "}"


instance Hashable RTrace where
    hashWithSalt i RTrace{_tKeyCode=kc, _tCost=c, _tSumPath=sp, _tRobots=rs} =
        i + fromIntegral kc + c + sp + sum (map hash rs)


hashRTraceParts :: KeyCode -> Int -> Int -> [Robot] -> Int
hashRTraceParts kc c sp rs =
    fromIntegral kc + c + sp + sum (map hash rs)


instance Eq RTrace where
    rt1@RTrace{_tHashForEquals=h1} == rt2@RTrace{_tHashForEquals=h2} =
        h1 == h2 &&
            (let RTrace{_tKeyCode=kc1, _tRobots=rs1} = rt1
                 RTrace{_tKeyCode=kc2, _tRobots=rs2} = rt2
              in kc1 == kc2 && all (uncurry (==)) (zip rs1 rs2))


instance Ord RTrace where
    compare RTrace{_tCost=c1, _tSumPath=p1} RTrace{_tCost=c2, _tSumPath=p2} =
        case compare c1 c2 of
            EQ -> compare p1 p2
            x -> x


-- create a trace from a set of robots.
makeTraceFromRobots :: [Robot] -> RTrace
makeTraceFromRobots rs = RTrace { _tKeyCode=keycode -- the combined path keycode
                                , _tCost=cost       -- the combined cost of all Robots
                                , _tSumPath=sp      -- the combined sum of paths
                                , _tHashForEquals=h -- the hash of the value for Equals
                                , _tRobots=rs
                                }
  where
      ks = map _robotCode rs
      keycode = foldr (DB..|.) 0x00 ks
      cost = sum $ map _robotCost rs
      sp = sum $ map (H.size . _robotHistory) rs
      h = hashRTraceParts keycode cost sp rs



-- store the keys and found as a KeyCode - 1 bit per word, ignoring the entrance
-- and starting for bit 0 as key a and bit 32 as door A
-- UPDATE: we don't care about doors; only keys.  Two key paths are equivalent
-- if they have the same keys as a path that doesn't include the door doesn't
-- matter.
charToKeyCode :: Char -> KeyCode
charToKeyCode c
  | v >= 65 && v <= 90 = 0x00  -- we don't actually care about doors -- only keys
  | v >= 97 && v <= 122 = 0x01 `DB.shift` (v - 97)
  | c == '@' = 0x00            -- the robot is just a space, so zero
  | otherwise = error "charToKeyCode only handles 'a-zA-Z'"
    where
        v = C.ord c


addCharToKeyCode :: Char -> KeyCode -> KeyCode
addCharToKeyCode c w = w DB..|. charToKeyCode c


-- convert a whole path to a word
stringToKeyCode :: String -> KeyCode
stringToKeyCode = foldr addCharToKeyCode 0x00


-- The priority Queue is a @Data.HashPSQ k p v@ where k is Coord, p is Int
-- (cost) and v is a Partial
type RQueue = Q.OrdPSQ KeyCode Int RTracker

-- a queue for the traces; by k=(hash Rtrace) p = Cost and v= RTrace
type TraceQueue = QH.HashPSQ RTrace Int RTrace

--
-- As we are going to be updating the Cache a LOT, we're going to make it a
-- mutable Array and run the whole calculation in an ST monad.  The priority
-- queue will bind the key (a Coord) to a Priority (an Int) and a Partial that
-- was the condition at that time.  Meanwhile, the Cache will store the best
-- partial so far, which may be better or worse than the partial on the priority
-- queue.
findSolutions :: Maze -> IO [(Int, [String])]
findSolutions maze = do
    let xys = findEntrances maze
    when (length xys /= 4) $ error $ "Only found " ++ show (length xys) ++ " entrances?"
    let keys = map snd $ filter (C.isLower.snd) $ findKeysAndDoors maze
        rs = map makeNewRobot xys
        initialTrace = makeTraceFromRobots rs
        initialTracker = RTracker { _keyCode=0x00
                                  , _cachedCosts=H.empty
                                  {-, _traces=[initialTrace]-}
                                  , _tracesQueue=QH.singleton initialTrace 0 initialTrace
                                  }
        initialState = SState { _bestCost=maxBound                        -- best cost found so far
                              , _pqueue=Q.singleton 0x00 0 initialTracker -- priority queue of trackers
                              , _steps=0
                              }
        reader = SReader { _maze=maze, _targetKeyCode=stringToKeyCode keys }
    putStrLn $ "keys=" ++ show keys
    putStrLn $ "initial Tracker" ++ show initialTracker
    putStrLn ""
    (s, w) <- RWS.execRWST processQueue reader initialState
    putStrLn $ "Number of steps was: " ++ show (_steps s)
    pure $ sortOn fst $ DL.toList w



-- the CostsCache is a Coord -> keycode -> cost
{-type CostsCache = H.HashMap Coord (H.HashMap KeyCode Int)-}

data SState = SState { _bestCost :: !Int    -- best cost found so far
                     , _pqueue :: !RQueue    -- priority queue of trackers
                     , _steps :: !Int        -- let's count the steps
                     }



data SReader = SReader { _maze :: !Maze
                       , _targetKeyCode :: !KeyCode
                       } deriving Show


-- the result of the computation; a set of paths of path length and string
type PQPaths = DL.DList (Int, [String])


-- The monad we run processQueue in
type SolutionT m a = RWS.RWST SReader PQPaths SState m a


-- execute the next lowest PQ item, which will call processTracker.  This
-- returns the list of paths in the Writer
processQueue :: SolutionT IO ()
processQueue = do
    SState{_bestCost=bestCost, _pqueue=queue, _steps=steps} <- RWS.get
    let next = Q.minView queue
    case next of
        Nothing -> pure ()   -- the paths are in the Writer DLList
        Just (_, cost, tracker, q') -> do
            {-RWS.lift $ putStrLn $ "processQueue: step:" ++ show steps ++ ", Cost=" ++ show cost ++ ", tracker is: " ++ show tracker-}
            {-RWS.lift getLine-}
            RWS.modify' $ \ss -> ss {_pqueue=q'}
            if cost >= bestCost
              then
                  processQueue
              else do
                  processTracker tracker
                  processQueue

processTracker :: RTracker -> SolutionT IO ()
{-processTracker RTracker{_traces=ts} | null ts = pure ()   -- no traces, then just return-}
processTracker RTracker{_tracesQueue=tq} | QH.null tq = pure ()   -- no traces, then just return
processTracker t@RTracker{_tracesQueue=tq, _keyCode=key, _cachedCosts=cCosts} = do
    let Just(_, cost, tr, tq') = QH.minView tq
        RTrace{_tKeyCode=cKey, _tCost=tCost, _tRobots=rs} = tr
{-processTracker t@RTracker{_traces=(tr@RTrace{ _tKeyCode=cKey      -- combined keycode-}
                                            {-, _tCost=tCost        -- the combined cost of all Robots-}
                                            {-, _tRobots=rs-}
                                            {-}:trs)-}
                         {-,_keyCode=key-}
                         {-,_cachedCosts=cCosts   -- the cached costs for this tracker/keycode-}
                         {-} = do-}
    when (cKey /= key) $ error "Something's wrong, the Trace code doesn't match the tracker."
    {-RWS.lift $ putStrLn $ "processTracker: tracker is: " ++ show t-}
    {-RWS.lift getLine-}
    SReader{_targetKeyCode=goal} <- RWS.ask
    SState{_bestCost=bestCost, _steps=steps} <- RWS.get
    RWS.modify' $ \ss -> ss {_steps=steps +1}    -- count how many times we've looped
    let isCheaper = areRobotsCheaperAtXYs2 rs cCosts
    if tCost > bestCost || not isCheaper
      {-then processTracker t {_traces=trs}   -- skip this one as it's more expensive that the best or cached.-}
      then processTracker t {_tracesQueue=tq'}   -- skip this one as it's more expensive that the best or cached.
      else do
        -- we have the best cost at this point so update tracker with the best
        -- cost
        let cCosts' = cacheCostsForRobots2 rs cCosts
        -- now generate candidates
        candidates <- deltaCandidateTraces tr  -- candidate traces (one robot move each)
        let (completePaths, remain) =
                partition (\RTrace{_tKeyCode=keysWord}
                  -> keysWord DB..&. goal == goal) candidates
            pathsFound = map (\RTrace{_tCost=cost, _tRobots=rrs} -> (cost, map _robotPath rrs)) completePaths

        -- if we've found a path, then add them to the paths found and update
        -- the minimu cost
        unless (null pathsFound) $ do
            forM_ pathsFound $ tell . DL.singleton  -- write any new paths to the Writer
            -- update the bestCost
            let minCost = minimum $ bestCost : map fst pathsFound
            RWS.modify' $ \ss -> ss {_bestCost=minCost}

        -- split out the Partials which are this tracker, vs other trackers.
        let (ourTs, otherTs) =
                partition (\RTrace{_tKeyCode=kc} -> kc == key) remain
            groupedTs = groupBy ((==) `on` _tKeyCode) otherTs

        -- put the other paths into other trackers
        forM_ groupedTs addTracesToTracker

        -- finally we process the rest of this Tracker
        {-let newTs = addTracesToTraces ourTs trs-}
        let newTq = addTracesToTracesQueue ourTs tq'
        {-processTracker t {_traces=newTs, _cachedCosts=cCosts'}-}
        processTracker t {_tracesQueue=newTq, _cachedCosts=cCosts'}


-- add a set of traces (all of the same keycode) to a track in the queue
addTracesToTracker :: [RTrace] -> SolutionT IO ()
addTracesToTracker [] = pure ()
addTracesToTracker ts@(RTrace{_tKeyCode=kc}:_) =
    RWS.modify' $ \ss@SState{_pqueue=queue} -> ss {_pqueue=snd $ Q.alter go kc queue}
  where
    go :: Maybe (Int, RTracker) -> ((), Maybe (Int, RTracker))
    -- create a new tracker for the key if the key doesn't exist
    go Nothing = ((), Just (minimum (map _tCost ts),
                            RTracker{ _keyCode=kc
                                    {-, _traces=addTracesToTraces ts []-}
                                    , _tracesQueue=addTracesToTracesQueue ts QH.empty
                                    , _cachedCosts=H.empty}))
    -- Update the existing tracker and cost with the new trace
    {-go (Just(pcost, t@RTracker{_traces=trs})) =-}
        {-((), Just(minimum (pcost:map _tCost ts), t {_traces=addTracesToTraces ts trs}))-}
    go (Just(pcost, t@RTracker{_tracesQueue=tq})) =
        ((), Just(minimum (pcost:map _tCost ts), t {_tracesQueue=addTracesToTracesQueue ts tq}))


-- add a trace to an existing Tracker or add a new tracker to the queue
{-addTraceToTracker :: RTrace -> SolutionT IO ()-}
{-addTraceToTracker tr@RTrace{_tKeyCode=kc, _tCost=cost} =-}
    {-RWS.modify' $ \ss@SState{_pqueue=queue} -> ss {_pqueue=snd $ Q.alter go kc queue}-}
  {-where-}
    {-go :: Maybe (Int, RTracker) -> ((), Maybe (Int, RTracker))-}
    {--- create a new tracker for the key if the key doesn't exist-}
    {-go Nothing = ((), Just (cost, RTracker{_keyCode=kc, _traces=[tr], _cachedCosts=H.empty}))-}
    {--- update the existing tracker and cost with the new trace-}
    {-go (Just (pcost, t@RTracker{_traces=trs})) =-}
        {-((), Just (minimum [cost, pcost], t {_traces=addTraceToTraces tr trs}))-}


{-addTracesToTraces :: [RTrace] -> [RTrace] -> [RTrace]-}
{-addTracesToTraces as bs = foldr addTraceToTraces bs as-}
addTracesToTracesQueue :: [RTrace] -> TraceQueue -> TraceQueue
addTracesToTracesQueue ts tq = foldr addTraceToTracesQueue tq ts


addTraceToTracesQueue :: RTrace -> TraceQueue -> TraceQueue
addTraceToTracesQueue rt@RTrace{_tCost=cost} tq =
    snd $ QH.alter go rt tq
  where
    go :: Maybe (Int, RTrace) -> ((), Maybe (Int, RTrace))
    -- create a new Trace for the key if it doesn't exist
    go Nothing = ((), Just (cost, rt))
    -- Update an existing RTrace.  We need to check the costs and work out
    -- whether to keep the old or replace it with the new one.
    go current@(Just(pcost, _)) =
        if cost < pcost
            then ((), Just(cost, rt))
            else ((), current)


-- the issue here is that we DON'T want to duplicate paths at locations; we only
-- want to keep the cheapest one and not add a partial that is at the same
-- position/path.  We'll use the pathword and xy to avoid adding the same item
-- and compare by cost.  We assume that ps is already sorted.
{-addTraceToTraces :: RTrace -> [RTrace] -> [RTrace]-}
{-addTraceToTraces = insertIntoList traceCompare traceEquals-}


{-traceEquals :: RTrace -> RTrace -> Bool-}
{-traceEquals RTrace{_tRobots=rs1} RTrace{_tRobots=rs2} =-}
    {-let xys1 = map _robotXY rs1-}
        {-xys2 = map _robotXY rs2-}
        {-ps1  = map _robotCode rs1-}
        {-ps2  = map _robotCode rs2-}
        {-sameCodes = zipWith (==) ps1 ps2-}
        {-sameXYs   = zipWith (==) xys1 xys2-}
     {-in all (uncurry (&&)) $ zip sameCodes sameXYs-}


{-traceCompare :: RTrace -> RTrace -> Ordering-}
{-traceCompare = compare `on` _tCost-}


showQueue :: RQueue -> IO ()
showQueue q = do
    putStrLn "Queue is:"
    forM_ (Q.toList q) $ \(k,cost,p) ->
        putStrLn $ "At: " ++ show k ++ ",  cost: " ++ show cost ++ ", tracker=" ++ show p
    putStrLn ""


-- Given a Trace, we want to generate all the possible further traces from here
-- using the n robots available.  i.e. each robot has 4 possible moves, which is
-- 16 possible traces.  Each trace has one robot move.  The returned list is a
-- list possible moves.  Each robot knows where it has been and so that is
-- eliminated if the position doesn't cost less (i.e. more keys) than the last
-- time it was there.
deltaCandidateTraces :: RTrace -> SolutionT IO [RTrace]
deltaCandidateTraces tr@RTrace{_tKeyCode=kc, _tRobots=rs} = do
    rrs <- forM rs (deltaCandidateRobots kc) -- [[Robot]] -- list of robot moves for each robot
    -- convert existing robots into [([], [])] so that we can build a list of
    -- Traces with the existing robots and that
    let sprs = splits rs   -- [([r],[r])] where r is Robot
        newRobotSets = map build $ concatMap merge $ zip sprs rrs -- [[Robot]]
        newTraces = map makeTraceFromRobots newRobotSets -- [RTrace]
    pure newTraces
  where
      build :: (([a],[a]), a) -> [a]
      build ((hs,ts),r) = hs ++ (r:ts)
      merge :: (([a],[a]), [a]) -> [(([a],[a]), a)]
      merge (p, as) = map (p,) as


-- from a Robot, generate up to 4 possible moves.  -- note they may not
-- necessarily be the cheapest for the whole RTrace that has 4 robots in them;
-- they are just the possible moves for that robot according to the shared
-- keycode and that robot
deltaCandidateRobots :: KeyCode -> Robot -> SolutionT IO [Robot]
deltaCandidateRobots kc r@Robot{_robotXY=xy, _robotHistory=history} = do
    maze <- RWS.asks _maze
    let newDeltas = deltas xy
        chars = map (maze DA.!) newDeltas
        pairs = zip newDeltas chars -- [(Coord, Char)]
        mCosts = map (`H.lookup` history) newDeltas -- [Maybe cost]
        robotDeltas = map (newRobotFromCoordChar r kc) pairs -- [Maybe Robot]
    pure $ M.catMaybes $ zipWith keepRobot mCosts robotDeltas -- [Robot]


-- this function asks "is this set of robots (at their xys) cheaper than what we
-- have cached.  Return True if they are.  Note this assumes that the cached
-- costs are for this keycode
areRobotsCheaperAtXYs2 :: [Robot] -> CachedCosts2 -> Bool
areRobotsCheaperAtXYs2 rs cc =
    let key = robotsToCacheKey rs
        cost = sum $ map _robotCost rs
        ccost = H.lookupDefault maxBound key cc
     in cost < ccost


robotsToCacheKey :: [Robot] -> (Coord, Coord, Coord, Coord)
robotsToCacheKey rs | length rs == 4 =
    let [c1, c2, c3, c4] = map _robotXY rs
     in (c1, c2, c3, c4)
robotsToCacheKey _ = error "Must pass 4 robots to robotsToCacheKey function!"


cacheCostsForRobots2 :: [Robot] -> CachedCosts2 -> CachedCosts2
cacheCostsForRobots2 rs cc =
    let key = robotsToCacheKey rs
        cost = sum $ map _robotCost rs
        ccost = H.lookupDefault maxBound key cc
     in if cost < ccost
          then H.insert key cost cc
          else cc


-- This function returns a new Partial from the old partial and the char/xy.
-- If the char is a Door, then the path is updated, but the _pathWord only
-- stores keys; this is so that we don't split at Doors, but we just go through
-- them.
newRobotFromCoordChar :: Robot -> KeyCode -> (Coord, Char) -> Maybe Robot
newRobotFromCoordChar r kc (xy, c)
    | c == '.' || c == '@' = newR
    -- key
    | isKey && hasCode = newR   -- is a key, but we already have the key
    | isKey = newRandC          -- is a key, but we don't already have it.
    -- door
    | isDoor && hasKey && hasDoor = newR -- is a door, we have the key, but we've seen it
    | isDoor && hasKey = newRandC       -- is a door, we have the key and we've not seen it
    | otherwise = Nothing                -- it's either a wall or a door with no key
  where
    keyCode = charToKeyCode c            -- note this ignores Doors; only codes for keys
    newCost = _robotCost r +1
    path = _robotPath r
    isKey = C.isLower c
    isDoor = C.isUpper c
    hasCode = kc DB..&. keyCode /= 0
    hasDoor = c `elem` path
    hasKey = kc DB..&. charToKeyCode (C.toLower c) /= 0
    newT = H.insert xy (robotToCost r) (_robotHistory r)
    newR = Just $ r { _robotXY=xy
                    , _robotCost=newCost
                    , _robotHistory=newT
                    , _robotBlocked=False}
    newRandC = Just $ r { _robotXY=xy
                        , _robotCost=newCost
                        , _robotPath=c:path
                        , _robotCode=_robotCode r DB..|. keyCode
                        , _robotBlocked=False
                        , _robotHistory=newT
                        }


keepRobot :: Maybe Cost -> Maybe Robot -> Maybe Robot
keepRobot _ Nothing = Nothing
keepRobot Nothing mr = mr
keepRobot (Just c) mr@(Just r) = if c < robotToCost r
                                   then Nothing
                                   else mr


-- utility functions
deltas :: Coord -> [Coord]
deltas xy = map (bothsum xy) [(1,0), (-1,0), (0,1), (0,-1)]

bothsum :: Num a => (a,a) -> (a,a) -> (a,a)
bothsum (a1,b1) (a2,b2) = (a1+a2,b1+b2)

findEntrance :: Maze -> Coord
findEntrance maze = fst $ head $ filter ((=='@').snd) $ DA.assocs maze


findEntrances :: Maze -> [Coord]
findEntrances maze = map fst $ filter ((=='@').snd) $ DA.assocs maze


findKeysAndDoors :: Maze -> [(Coord,Char)]
findKeysAndDoors maze = filter (C.isAlpha . snd) $ DA.assocs maze

-- test mazes

maze1Txt :: [T.Text]
maze1Txt = [ "#######"
           , "#a.#Cd#"
           , "##...##"
           , "##.@.##"
           , "##...##"
           , "#cB#.b#"
           , "#######"
           ]

maze1 :: ([T.Text], Int)
maze1 = (maze1Txt, 8)


maze2Txt :: [T.Text]
maze2Txt = [ "###############"
           , "#d.ABC.#.....a#"
           , "######...######"
           , "######.@.######"
           , "######...######"
           , "#b.....#.....c#"
           , "###############"
           ]

maze2 :: ([T.Text], Int)
maze2 = (maze2Txt, 24)


maze3Txt :: [T.Text]
maze3Txt = [ "#############"
           , "#DcBa.#.GhKl#"
           , "#.###...#I###"
           , "#e#d#.@.#j#k#"
           , "###C#...###J#"
           , "#fEbA.#.FgHi#"
           , "#############"
           ]

maze3 :: ([T.Text], Int)
maze3 = (maze3Txt, 32)


maze4Txt :: [T.Text]
maze4Txt = [ "#############"
           , "#g#f.D#..h#l#"
           , "#F###e#E###.#"
           , "#dCba...BcIJ#"
           , "#####.@.#####"
           , "#nK.L...G...#"
           , "#M###N#H###.#"
           , "#o#m..#i#jk.#"
           , "#############"
           ]

maze4 :: ([T.Text], Int)
maze4 = (maze4Txt, 72)



loadTestMaze :: [T.Text] -> Maze
loadTestMaze = readMap . T.unlines


testItWith :: ([T.Text], Int) -> IO ()
testItWith (txt, n) = do
    putStrLn $ intercalate "\n" $ map T.unpack txt
    let maze = loadTestMaze txt
        maze' = adjustMaze maze
    putStrLn "Before"
    putStrLn $ intercalate "\n" $ mazeToString maze
    putStrLn "After"
    putStrLn $ intercalate "\n" $ mazeToString maze'
    print $ findEntrances maze'
    putStrLn $ "Goal is: " ++ show n
    solns <- findSolutions maze'
    print solns


main36 :: IO ()
main36 = do
    putStrLn "Day 18: Part 2: 4 damn cooperating maze robots ..."
    mazeTxt <- loadMaze
    putStrLn $ intercalate "\n" $ lines $ T.unpack mazeTxt
    let maze = readMap mazeTxt
        maze' = adjustMaze maze
    {-let maze = loadTestMaze maze4Txt-}
        {-maze'= adjustMaze maze-}
    putStrLn $ intercalate "\n" $ mazeToString maze'
    putStrLn "Solutions "
    solns <- findSolutions maze'
    print solns
