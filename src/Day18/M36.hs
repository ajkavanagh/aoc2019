{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}

module Day18.M36 where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Control.Monad            (forM, forM_, mapM_, unless, when,
                                           (>=>))
import qualified Control.Monad.RWS.Strict as RWS

import           Data.Array               (Array)
import qualified Data.Array               as DA

import qualified Data.Bits                as DB
import           Data.Word                (Word64)

import qualified Data.Char                as C
import qualified Data.DList               as DL
import           Data.List                (delete, find, foldl', intercalate,
                                           lookup, nub, partition, sort, sortOn)
import           Data.List.Split          (chunksOf)
import qualified Data.Maybe               as M

import qualified Data.HashMap.Strict      as H

import qualified Data.OrdPSQ              as Q

import           Utils                    (splits)

import           Text.Printf              (printf)
-- debuging
import           Debug.Trace              (trace)


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
        adjust = map (\(xy', c) -> (bothsum xy xy', c))
                     (zip wall (repeat '#') ++ [((0,0), '#')] ++ zip corners (repeat '@'))
     in maze DA.// adjust


mazeToString :: Maze -> [String]
mazeToString maze =
    let ((_,ymin),(_,ymax)) = DA.bounds maze
     in chunksOf (ymax - ymin +1) $ DA.elems maze


mazeToStringWithNums :: Maze -> [String]
mazeToStringWithNums maze =
    let ss = mazeToString maze
        sx = length ss
        sy = length (head ss)
        nDigits n = 1 + ((floor $ logBase 10.0 (fromIntegral n)) :: Int)
        nly = nDigits sy    -- number of lines to leave
        nlx = nDigits sx    -- number of chars for x
        spaces x = replicate x ' '
        heading :: Int -> String
        heading pow =
            let m = (floor $ 10 ** fromIntegral pow) :: Int
                charAt y' = if y' < m then ' ' else head $ show ((y' `div` m) `mod` 10)
             in map charAt [0..sy-1]
        headings :: [String]
        headings = [heading p | p <- reverse [0..nly-1]]
        -- add space to the headings at the front and one line afterwards
        header = map (spaces (nlx+1) ++) headings ++ [spaces (nlx + 1 + sy)]
        formatn n =
            let p = show n
                w = length p
             in spaces (nlx - w) ++ p ++ " "
        ss' = zip [0..] ss
        lines = map (\(n, l) -> formatn n ++ l) ss'
     in header ++ lines



-- Let's represent the corners of the maze as a Graph of Nodes.  The Nodes will
-- Graph will hold the nodes and the edges, and there will be an Index into the
-- Graph by Coord and Key separately.


-- we actually keep the Nodes in an ID -> Node lookup so that we modify the
-- nodes by still keep a 'reference' to them.

data Graph = Graph { _start  :: !Int                       -- The node ID to start at
                   , _keys   :: !(H.HashMap Char Int)      -- Each node has an ID
                   , _coords :: !(H.HashMap Coord Int)   -- The ID is unique to the Node
                   , _nodes  :: !(H.HashMap Int Node)     -- Look up a Node via an Int
                   } deriving Show

data Node = Node { _nodeId :: !Int
                 , _coord  :: !Coord   -- where it is, which is only useful to humans
                 , _char   :: !Char     -- what's there (is it a key for example)
                 , _edges  :: ![Edge]  -- and what leaves that node in all directions, sorted by cost
                 } deriving Show

data Edge = Edge { _cost       :: !Int           -- how expensive it is
                 , _constraint :: !String  -- what keys are needed for this edge
                 , _node       :: !(Maybe Int)    -- and where it goes (as a Node)
                 } deriving (Show, Eq)


-- derive the 4 graphs from the maze and return them as a list
-- This is what we derive the solution from
graphsFromMaze :: Maze -> [Graph]
graphsFromMaze maze =
    map ( removeCornersRecursively
        . removeDeadEndsRecursively
        . graphFromTrace
        . traceMazeFrom maze
        ) $ findEntrances maze


graphFromTrace :: [(String, (Coord, Coord))] -> Graph
graphFromTrace = graphFromNodes . traceToNodes


graphFromNodes :: [Node] -> Graph
graphFromNodes ns =
    let keyNodes = filter (C.isLower._char) ns
     in Graph { _start = (_nodeId.head) $ filter ((=='@')._char) ns
              , _keys = H.fromList $ map (\n -> (_char n, _nodeId n)) keyNodes
              , _coords = H.fromList $ map (\n -> (_coord n, _nodeId n)) ns
              , _nodes = H.fromList $ map (\n -> (_nodeId n, n)) ns
              }

-- get a node using an ID
nodeForId :: Graph -> Int -> Node
nodeForId g i = _nodes g H.! i


-- get a node using a Coord
nodeAtCoord :: Graph -> Coord -> Node
nodeAtCoord g xy = nodeForId g $ _coords g H.! xy


-- get a node using a key
nodeForKey :: Graph -> Char -> Node
nodeForKey g c = nodeForId g $ _keys g H.! c

-- Nodes with one edge and start with a '.'
deadEnds :: Graph -> [Node]
deadEnds g =
    let ns = map snd $ H.toList $ _nodes g
        ds = filter (\n -> length (_edges n) == 1 && _char n == '.') ns
     in ds


-- remove a DeadEnd -- go to the node that it points to, remove the edge from
-- that node that points back to the deadend, re-write the node, and remove the
-- deadend node, and return a new graph
-- Note that it also has to adjust the _coords entry of the graph
removeDeadEnd :: Graph -> Node -> Graph
removeDeadEnd g n =
    let targetNode = (nodeForId g. M.fromMaybe undefined ._node.head._edges) n
        nId = _nodeId n
        isNode mid = case mid of
            Just id -> nId == id
            Nothing -> False
        newEdges = filter (not.mEquals nId._node) (_edges targetNode)
        targetNode' = targetNode { _edges=newEdges }
        -- remove the old node
        nodes = _nodes g
        nodes' = H.delete nId nodes
        -- update the Node it used to go to
        nodes'' = H.insert (_nodeId targetNode') targetNode' nodes'
        -- now delete the coord mapping
        coords' = H.delete (_coord n) (_coords g)
     in g {_nodes=nodes'', _coords=coords'}


mEquals :: Eq a => a -> Maybe a -> Bool
mEquals a mb = case mb of
    Nothing -> False
    Just b  -> a == b


removeDeadEnds :: Graph -> [Node] -> Graph
removeDeadEnds = foldl' removeDeadEnd


removeDeadEndsRecursively :: Graph -> Graph
removeDeadEndsRecursively g =
    case deadEnds g of
        [] -> g
        ds -> removeDeadEndsRecursively (removeDeadEnds g ds)

-- Now remove nodes which are just effectively now straight lines
-- first detect them.  We want a node that is a '.' and only has two edges.
-- These will need to be joined.
detectCorners :: Graph -> [Node]
detectCorners g =
    let ns = map snd $ H.toList $ _nodes g
        cs = filter (\n -> length (_edges n) == 2 && _char n == '.') ns
     in cs


-- Removing a corner node. We need the only two nodes from the edges.
-- We then need to find the edges that point to this node. e.g.
--
--     A ---e1--> B ---e2--> C
--     A <--e4--- B <--e3--- C
--
-- the e1 needs to be extended to C by adding e2 to it's cost, joining the
-- constraints and changing the target Node for C into e1, and e3 needs to do
-- the same.  Remember 'B' is a '.' node, NOT a key.
removeCorner :: Graph -> Node -> Graph
removeCorner g bn = g {_coords=coords', _nodes=nodes'''}
  where
    mBId = _nodeId bn
    [an, cn] = map (nodeForId g. M.fromMaybe undefined ._node) $ _edges bn
    edgeFor i n = head $ filter (mEquals i . _node) $ _edges n
    -- find the edge for an that points to bn from a and c
    e1 = edgeFor mBId an
    e3 = edgeFor mBId cn
    -- find the edge that goes from B to C
    e2 = edgeFor (_nodeId cn) bn
    e4 = edgeFor (_nodeId an) bn
    -- now create a new e1 and e3 using the data from e2 and e4
    e1' = Edge { _cost=_cost e1 + _cost e2
               , _constraint=_constraint e1 ++ _constraint e2
               , _node=_node e2
               }
    e3' = Edge { _cost=_cost e3 + _cost e4
               , _constraint=_constraint e3 ++ _constraint e4
               , _node=_node e4
               }
    -- delete the bn node from the graph
    nodes = _nodes g
    nodes' = H.delete mBId nodes
    -- now delete the coord mapping for the bn node
    coords' = H.delete (_coord bn) (_coords g)
    -- Now update the an and cn nodes with the new e1' and e3' edges
    -- first update the an and c2
    aEdges' = e1' : delete e1 (_edges an)
    cEdges' = e3' : delete e3 (_edges cn)
    -- create new an and cn
    an' = an {_edges = aEdges'}
    cn' = cn {_edges = cEdges'}
    -- and build that back into the nodes
    nodes'' = H.insert (_nodeId an') an' nodes'
    nodes''' = H.insert (_nodeId cn') cn' nodes''


-- remove corners until there are no corners
removeCornersRecursively :: Graph -> Graph
removeCornersRecursively g =
    case detectCorners g of
        []    -> g
        (n:_) -> removeCornersRecursively (removeCorner g n)

--

makeEdgeFromPath :: String -> Maybe Int -> Edge
makeEdgeFromPath s mi =
    let n = length s -1
        constraint = filter C.isUpper s
     in Edge {_cost=n, _constraint=constraint, _node=mi}


-- make a Node and also the bits necessary to make the edges; but they can't be
-- added to the Node until all the Nodes are made
makeNode :: [(String, (Coord, Coord))]
         -> [(Coord, Char)]
         -> (Int, Coord)
         -> (Node, [(String, (Coord, Coord))])
makeNode ts ls (i, xy) =
    let ss = filter ((==xy).(fst.snd)) ts  -- filter by the first coord  -- these are the paths that start from this node
        es = filter ((==xy).(snd.snd)) ts -- filter by the second coord -- these are the paths that END at this node
        es' = map (\(s,(a,b)) -> (reverse s, (b,a))) es  -- flip the ENDs so they look like starts
        c = M.fromMaybe undefined $ lookup xy ls
     in ( Node { _nodeId=i
               , _coord=xy
               , _char=c
               , _edges=[]     -- can't fill this in yet untill all the nodes are available
               }
        , ss ++ es')


-- finish building the Nodes by constructing the edges from the
-- [(String, (Coord, Coord))] list, and inserting them into the _edges of the
-- Node
finishNode :: [Node] -> (Node, [(String, (Coord, Coord))]) -> Node
finishNode ns (node, protoEdges) =
    let nodeId = _nodeId node
        finder xy = find ((==xy)._coord) ns                -- Coord -> Maybe Node
        {-mNodes = map (finder.snd.snd) protoEdges           -- [Maybe Node]-}
        nodeInts = map ((_nodeId <$>).(finder.snd.snd)) protoEdges -- [Maybe Int]
        ps    = map fst protoEdges                         -- [String]
        pathNodePairs = zip ps nodeInts
        edges = zipWith makeEdgeFromPath ps nodeInts     -- [Edge]
     in node { _edges=edges }



-- convert the trace into the Nodes and a list of (Int, Coord) where the Coord
-- is the 'end' of the edge that the String referred to.
traceToNodes :: [(String, (Coord, Coord))] -> [Node]
traceToNodes ts =
    -- we want a unique set of coords -- these will be the nodes.
    let cs = nub $ sort $ map (fst . snd) ts ++ map (snd . snd) ts  -- [Coord]
        -- now pair each unique coord with an ascending list of ints
        ls = traceToCoordChar ts            -- [(Coord, Char)]
        ps = zip [1..] cs                   -- [(Int, Coord)]
        pNodes = map (makeNode ts ls) ps       -- [(Node, [(String, (Coord, Coord))])]
        -- Now we have a list of (Nodes, ts), where ts is the filtered set of paths against that Node
        -- now we have all the nodes we need to convert the remaining paths into
        -- edges that can hook back to those nodes
        ns = map fst pNodes   -- [Node] ; but without edges
        nodes = map (finishNode ns) pNodes   -- [Node]
     in nodes


-- convert the trace to a lookup list of coords -> chars
traceToCoordChar :: [(String, (Coord, Coord))] -> [(Coord, Char)]
traceToCoordChar ts =
    let left (p, (xy,_)) = (xy, head p)
        right (p, (_, xy)) = (xy, last p)
     in nub $ sort $ map left ts ++ map right ts



traceMazeFrom :: Maze -> Coord -> [(String, (Coord, Coord))]
traceMazeFrom maze xy = traceMazeFrom' maze [xy] ("@", (xy, xy))


traceMazeFrom' :: Maze
               -> [Coord]
               -> (String, (Coord, Coord))
               -> [(String, (Coord, Coord))]
--traceMazeFrom' m xys c | trace ("traceMazeFrom': " ++ show m ++ ", " ++ show xys ++ ", " ++ show c) False = undefined
traceMazeFrom' maze xys (path, (sXY, cXY)) =
    let newDeltas = deltas cXY
        keepDeltas = filter (`notElem` xys) newDeltas
        chars = map (maze DA.!) keepDeltas
        pairs = zip keepDeltas chars  -- [(Coord, Char)]
        pairs' = filter ((/='#').snd) pairs   -- ignore walls from options
        nOptions = length pairs'
     in if nOptions > 1 && sXY /= cXY
          -- we've hit an intersection.  Make a path and continue with both
          then (reverse path, (sXY, cXY)) : concatMap (traceMazeHelper maze xys ([head path], (cXY, cXY))) pairs'
          -- just continue down the path
          else concatMap (traceMazeHelper maze xys (path, (sXY, cXY))) pairs'


traceMazeHelper :: Maze
                -> [Coord]
                -> (String, (Coord, Coord))
                -> (Coord, Char)
                -> [(String, (Coord, Coord))]
traceMazeHelper maze xys (path, (sXY, cXY)) (xy, char)
    -- it's a cooridoor path so just conintue
    | char == '.' = traceMazeFrom' maze (xy:xys) (char:path, (sXY, xy))
    -- it's a key, so this is a Node
    | C.isLower char = (reverse (char:path), (sXY, xy)) : traceMazeFrom' maze (xy:xys) ([char], (xy, xy))
    -- it's a door, so just add it to the path
    | C.isUpper char = traceMazeFrom' maze (xy:xys) (char:path, (sXY, xy))
    -- otherwise it's a door or something else, so just drop it
    | otherwise = []


-- And now the solution solver:
--
-- This takes the 4 graphs and then try to find the shortest path using a
-- priority queue of possible solutions.  However, we DON'T want to actually
-- generate all options for each location.  And that's basically the problem;
-- how to select the next minimum without having to generate millions of options
-- at each stage; how do we decide which one's not NOT put on the PQ.

type KeyCode = Word64

data Robot = Robot { _robotPath    :: !String   -- full path by this robot
                   , _robotCost    :: !Int      -- cost of the path (number of steps)
                   , _robotNodeId  :: !Int      -- where it is -- the Node Id
                   , _robotCode    :: !KeyCode  -- the partial code stored in this robot
                   , _robotHistory :: !(H.HashMap Int Cost)  -- where it has been (with costs)
                   }

instance Show Robot where
    show r@Robot{ _robotPath=p
                , _robotCost=c
                , _robotNodeId=nodeId
                , _robotCode=kc
                , _robotHistory=h
                }
        =  "R[" ++ show p ++ ", cost=" ++ show c ++ ", node:" ++ show nodeId
        ++ ", history-len=" ++ show (H.size h)
        ++ ", partial code=" ++ printf "0x%08x" kc
        ++ ", pathCost here=" ++ show (robotToCost r)
        ++ ", history=" ++ intercalate ", " (map show (H.toList h))
        ++ "]"


-- two robots are equal if their code and xy locations are the same.  They are
-- not 'equal' based on their path or history; this is how we elimiate worse
-- robots.
instance Eq Robot where
    Robot{_robotCode=kc1, _robotNodeId=n1} == Robot{_robotCode=kc2, _robotNodeId=n2} =
        kc1 == kc2 && n1 == n2


makeNewRobot :: Int -> Robot
makeNewRobot nodeId = Robot { _robotPath=""
                            , _robotCost=0
                            , _robotNodeId=nodeId
                            , _robotCode=0x00
                            , _robotHistory=H.empty
                            }


-- a cost element = (path length, Cost): used to stop Robots backtracking where
-- they came from unless they have more keys/doors.  The first part of the pair
-- is the number of keys that robot has, the second is the current cost at that
-- point in it's life
newtype Cost = Cost (KeyCode, Int) deriving (Show, Eq)


instance Ord Cost where
    compare (Cost (k1, c1)) (Cost (k2, c2)) = case compare k2 k1 of
        EQ -> compare c1 c2
        x  -> x

--
-- the cost is ONLY the keys; i.e. we don't record backtracking by a robot for a
-- Door.  -- this is to prevent backtracking from a location
robotToCost :: Robot -> Cost
robotToCost Robot{_robotCost=cost, _robotCode=p} = Cost (p, cost)


data RState = RState { _tKeyCode :: !KeyCode -- the combined path keycode
                     , _tCost    :: !Int   -- the combined cost of all Robots
                     , _tSumPath :: !Int -- the combined sum of all path lengths
                     , _tRobots  :: ![Robot]  -- and where are all the robots?
                     }

instance Show RState where
    show RState{_tKeyCode=code, _tCost=c, _tRobots=rs}
      =  "RState{" ++ printf "0x%08x" code ++ ", cost=" ++ show c
      ++ ", robots=\n" ++ intercalate "\n" (map show rs)
      ++ "}"





-- create a trace from a set of robots.
makeRStateFromRobots :: [Robot] -> RState
makeRStateFromRobots rs = RState { _tKeyCode=keycode -- the combined path keycode
                                 , _tCost=cost       -- the combined cost of all Robots
                                 , _tSumPath=sp      -- the combined sum of paths
                                 , _tRobots=rs
                                 }
  where
      ks = map _robotCode rs
      keycode = foldr (DB..|.) 0x00 ks
      cost = sum $ map _robotCost rs
      sp = sum $ map (H.size . _robotHistory) rs


-- store the keys and found as a KeyCode - 1 bit per word, ignoring the entrance
-- and starting for bit 0 as key a and bit 32 as door A
-- UPDATE: we don't care about doors; only keys.  Two key paths are equivalent
-- if they have the same keys as a path that doesn't include the door doesn't
-- matter.
charToKeyCode :: Char -> KeyCode
charToKeyCode c
  | v >= 65 && v <= 90 = 0x00  -- we don't actually care about doors -- only keys
  | v >= 97 && v <= 122 = 0x01 `DB.shift` (v - 97)
  | c == '@' || c == '.' = 0x00            -- the robot is just a space, so zero
  | otherwise = error "charToKeyCode only handles 'a-zA-Z'"
    where
        v = C.ord c


addCharToKeyCode :: Char -> KeyCode -> KeyCode
addCharToKeyCode c w = w DB..|. charToKeyCode c


doorToKeyCode :: Char -> KeyCode
doorToKeyCode c
  | C.isUpper c = charToKeyCode (C.toLower c)
  | otherwise = 0x00


-- convert a whole path to a word
stringToKeyCode :: String -> KeyCode
stringToKeyCode = foldr addCharToKeyCode 0x00


-- convert a list of doors to a matching set of keys
doorsToKeyCode :: String -> KeyCode
doorsToKeyCode = foldr addDoorToKeyCode 0x00
  where
      addDoorToKeyCode c w = w DB..|. doorToKeyCode c


hasKeys :: KeyCode -> String -> Bool
hasKeys _ "" = True
hasKeys kc s =
    let rc = doorsToKeyCode s
     in rc DB..&. kc == rc


-- The Priority Queue (PQ) is going to be keyed by the four node positions and
-- the keycode.  That type will be a PQKey

type PQKey = (Int, Int, Int, Int, KeyCode)

-- The priority Queue is a @Data.HashPSQ k p v@ where k is Coord, p is Int
-- (cost) and v is a Partial
type RQueue = Q.OrdPSQ PQKey Int RState


rStateToPQKey :: RState -> PQKey
rStateToPQKey RState{_tKeyCode=kc, _tRobots=rs} =
    let [n1,n2,n3,n4] = map _robotNodeId rs
     in (n1,n2,n3,n4,kc)


-- the is the queue update function; it takes an RState and updates it into the
-- queue
updateRQueueWithRState :: RQueue -> RState -> RQueue
updateRQueueWithRState rq rs@RState{_tCost=cost} = snd $ Q.alter go kc rq
  where
    kc = rStateToPQKey rs
    go :: Maybe (Int, RState) -> ((), Maybe (Int,RState))
    -- create a new entry if the key doesn't exist
    go Nothing = ((), Just (cost, rs))
    -- if an existing key of the same exists, check against the cost of this one
    -- and if better, keep the old one; otherwise, use the new one.
    go current@(Just(pcost, _)) =
        if cost < pcost
            then ((), Just(cost, rs))
            else ((), current)


data SState = SState { _bestCost :: !Int    -- best cost found so far
                     , _pqueue   :: !RQueue    -- priority queue of trackers
                     , _steps    :: !Int        -- let's count the steps
                     }



data SReader = SReader { _graphs        :: ![Graph]
                       , _maze          :: !Maze
                       , _targetKeyCode :: !KeyCode
                       } deriving Show


-- the result of the computation; a set of paths of path length and string
type PQPaths = DL.DList (Int, [String])


-- The monad we run processQueue in
type SolutionT m a = RWS.RWST SReader PQPaths SState m a


-- The findSolutions function which produces a list of (Int, [Paths]) that
-- indicate the solutions.
-- It sets up the SolutionT and then calls the processQueue which actually
-- iterates the RQueue until it is done - which indicates that the search is
-- complete.
--
findSolutions :: Maze -> IO [(Int, [String])]
findSolutions maze = do
    let xys = findEntrances maze
    when (length xys /= 4) $ error $ "Only found " ++ show (length xys) ++ " entrances?"
    let keys = map snd $ filter (C.isLower.snd) $ findKeysAndDoors maze
        gs = graphsFromMaze maze
        rs = map (makeNewRobot._start) gs
        initialRstate = makeRStateFromRobots rs
        initialState = SState { _bestCost=maxBound                        -- best cost found so far
                              , _pqueue=updateRQueueWithRState Q.empty initialRstate
                              , _steps=0
                              }
        reader = SReader { _graphs=gs, _maze=maze, _targetKeyCode=stringToKeyCode keys }
    putStrLn $ "keys=" ++ show keys
    putStrLn $ "initial RState" ++ show initialRstate
    putStrLn ""
    (s, w) <- RWS.execRWST processQueue reader initialState
    putStrLn $ "Number of steps was: " ++ show (_steps s)
    pure $ sortOn fst $ DL.toList w


-- Now process the first item on the queue, generate the next set of options,
-- determine whether to use them by adding them speculatively to the queue and
-- repeat until the queue is empty
processQueue :: SolutionT IO ()
processQueue = do
    SState{_bestCost=bestCost, _pqueue=queue, _steps=steps} <- RWS.get
    let next = Q.minView queue
    case next of
        Nothing -> pure ()   -- the paths are in the Writer DLList
        Just (_, cost, rs, q') ->
            if cost > bestCost
              then do
                RWS.modify' $ \ss -> ss {_pqueue=q', _steps=steps +1}
                processQueue
              else do
                candidates <- generateOptions rs

                SReader{_targetKeyCode=goal, _maze=maze} <- RWS.ask
                let (completePaths, remain) = partition (\RState{_tKeyCode=keysWord}
                        -> keysWord DB..&. goal == goal) candidates
                    pathsFound = map (\RState{_tCost=cost, _tRobots=rrs}
                        -> (cost, map _robotPath rrs)) completePaths

                -- if we've found a path, then add them to the paths found and update
                -- the minimum cost
                unless (null pathsFound) $ do
                    forM_ pathsFound $ RWS.tell . DL.singleton  -- write any new paths to the Writer
                    -- update the bestCost
                    let minCost = minimum $ bestCost : map fst pathsFound
                    RWS.modify' $ \ss -> ss {_bestCost=minCost}

                -- now add any options to the tracker -- note they should have
                -- been culled by bestCost, so, we should only add stuff that
                -- can still be possibly cheaper
                let q'' = foldl' updateRQueueWithRState q' remain
                RWS.modify' $ \ss -> ss {_pqueue=q'', _steps=steps +1}
                processQueue


showQueue :: RQueue -> IO ()
showQueue q = do
    putStrLn $ "Queue is: (length):" ++ show (Q.size q)
    forM_ (take 4 $ Q.toList q) $ \(k,cost,p) ->
        putStrLn $ "At: " ++ show k ++ ",  cost: " ++ show cost ++ ", RState=" ++ show p
    putStrLn ""

-- generateOptions -- generate a list of RState options from the current option;
-- n for each of the robots assuming ONE move per robot.  These are culled
-- against the best cost; the act of inserting them into the queue determines
-- whether they are kept based on there cost.
generateOptions :: RState -> SolutionT IO [RState]
generateOptions r@RState{_tRobots=rs, _tKeyCode=kc} = do
    SState{_bestCost=bestCost} <- RWS.get
    graphs <- RWS.asks _graphs
    let pairs = zip rs graphs
    rrs <- forM pairs (generateOptionsForRobot kc)   -- [[Robot]] list of moves for each robot
    -- convert existing robots into [([], [])] so that we can build a list of
    -- Traces with the existing robots and that
    let sprs = splits rs   -- [([r],[r])] where r is Robot
        newRobotSets = map build $ concatMap merge $ zip sprs rrs -- [[Robot]]
        newRStates = map makeRStateFromRobots newRobotSets -- [RTrace]
        fRStates = filter (\RState{_tCost=c} -> c < bestCost) newRStates
    pure fRStates
  where
      build :: (([a],[a]), a) -> [a]
      build ((hs,ts),r) = hs ++ (r:ts)
      merge :: (([a],[a]), [a]) -> [(([a],[a]), a)]
      merge (p, as) = map (p,) as


generateOptionsForRobot :: KeyCode -> (Robot, Graph) -> SolutionT IO [Robot]
generateOptionsForRobot kc (r@Robot{_robotNodeId=n, _robotHistory=h}, g) = do
    let node = _nodes g H.! n
        -- keep edges for which we have a key
        edges = filter (hasKeys kc . _constraint) $ _edges node
        mCosts = map (_node >=> (`H.lookup` h)) edges  -- [Maybe Cost]
        robotOptions = map (newRobotFromEdge g r) edges  -- [Maybe Robot]
    pure $ M.catMaybes $ zipWith keepRobot mCosts robotOptions


newRobotFromEdge :: Graph -> Robot -> Edge -> Maybe Robot
newRobotFromEdge g r@Robot{ _robotCost=rCost
                          , _robotPath=p
                          , _robotNodeId=rNode
                          , _robotCode=kc
                          , _robotHistory=h}
                 Edge{ _cost=eCost
                     , _constraint=constraint
                     , _node=mInt} =
    let mNode = (_nodes g H.!) <$> mInt
        char = _char <$> mNode      -- Maybe Char
     in case char of
        Nothing -> Nothing
        Just c ->
            let isKey = C.isLower c
                newH = H.insert (M.fromMaybe undefined mInt) (robotToCost r) h
                newKc = addCharToKeyCode c kc
             in mInt >>= \i ->
                pure Robot { _robotCost=rCost + eCost
                           , _robotPath=p ++ constraint ++ (if isKey then [c] else "")
                           , _robotNodeId=i
                           , _robotCode=newKc
                           , _robotHistory=newH
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
    let es = findEntrances maze'
    print es
    let routes = map (traceMazeFrom maze') es
    mapM_ print routes
    putStrLn $ "Goal is: " ++ show n
    solns <- findSolutions maze'
    print solns



main36 :: IO ()
main36 = do
    putStrLn "Day 18: Part 2: 4 damn cooperating maze robots ..."
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
        maze' = adjustMaze maze
    putStrLn $ intercalate "\n" $ take 44 $ mazeToStringWithNums maze'
    putStrLn "Solutions "
    solns <- findSolutions maze'
    print solns


