{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}

module Day18.M35 where

import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Control.Monad              (forM_, unless, filterM)
import qualified Control.Monad.RWS.Strict   as RWS

import           Data.Array                 (Array)
import qualified Data.Array                 as DA

import           Data.Word                  (Word64)
import qualified Data.Bits                  as DB

import qualified Data.Char                  as C
import           Data.List                  (intercalate, partition)
import           Data.List.Split            (chunksOf)
import qualified Data.DList                 as DL
import qualified Data.Maybe                 as M

import qualified Data.HashMap.Strict        as H

import qualified Data.OrdPSQ                as Q

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


mazeToString :: Maze -> [String]
mazeToString maze =
    let ((xmin,_),(xmax,_)) = DA.bounds maze
     in chunksOf (xmax - xmin +1) $ DA.elems maze


-- Let's have a data item.  it's a path, the number of keys found, and the cost
-- to traverse that path.

data Tracker = Tracker { _keycode :: !KeyCode
                       , _partials :: ![Partial]
                       }

instance Show Tracker where
    show Tracker{_keycode=key, _partials=ps}
      = "T(code=" ++ show key
      ++ ", partials:[" ++ intercalate ", " (map showMinPartial ps)
      ++ "])"


type KeyCode = Word64

data Partial = Partial { _path  :: !String  -- path is reversed
                       , _pathWord :: !KeyCode -- path as bits
                       , _cost  :: !Int
                       , _coord :: !Coord
                       , _traversal :: !(H.HashMap Coord Cost)
                       } deriving (Eq)


instance Show Partial where
    show Partial{_path=p, _cost=c, _coord=xy, _traversal=t, _pathWord=pw}
      = "P(" ++ show p
      ++ ", c:" ++ show c
      ++ ", at: " ++ show xy
      ++ ", len: " ++ show (H.size t)
      ++ ", keyCode=" ++ printf "0x%08x" pw
      {-++ ", path is=" ++ show t-}
      ++ ")"

showMinPartial :: Partial -> String
showMinPartial Partial{_path=p, _cost=c, _coord=xy, _traversal=t, _pathWord=pw}
  = "P(" ++ printf "0x%08x" pw ++ ", at: " ++ show xy ++ ", c:" ++ show c ++ ")"



-- A Partial is Ord, and we'll implement Ord for Partial so we can compare them.
-- A LT partial is one with more keys and a lower cost.  So we compare
-- number of keys and then cost for a partial.
-- e.g. Partial{_path="", _numKeys=3, _cost=4} < Partial{_path="", _numKeys=2, _cost=3}
instance Ord Partial where
    compare Partial{_path=p1, _cost=c1} Partial{_path=p2, _cost=c2} =
        case compare (length p2) (length p1) of
            EQ -> compare c1 c2
            x  -> x


partialToCost :: Partial -> Cost
partialToCost Partial{_cost=cost, _path=p} = Cost (length p, cost)

-- a cost element = (path length, Cost)
newtype Cost = Cost (Int, Int) deriving (Show, Eq)

instance Ord Cost where
    {-compare (Cost (k1, c1)) (Cost (k2, c2)) = case compare k2 k1 of-}
        {-EQ -> compare c1 c2-}
        {-x -> x-}
    compare (Cost (k1, c1)) (Cost (k2, c2)) = case compare k2 k1 of
        EQ -> compare c1 c2
        x -> x


-- store the keys and found as a KeyCode - 1 bit per word, ignoring the entrance
-- and starting for bit 0 as key a and bit 32 as door A
-- UPDATE: we don't care about doors; only keys.  Two key paths are equivalent
-- if they have the same keys as a path that doesn't include the door doesn't
-- matter.
charToKeyCode :: Char -> KeyCode
charToKeyCode c
  -- | v >= 65 && v <= 90 = 0x10000 `DB.shift` (v - 65)
  | v >= 65 && v <= 90 = 0x00  -- we don't actually care about doors -- only keys
  | v >= 97 && v <= 122 = 0x01 `DB.shift` (v - 97)
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
type TQueue = Q.OrdPSQ KeyCode Int Tracker

--
-- As we are going to be updating the Cache a LOT, we're going to make it a
-- mutable Array and run the whole calculation in an ST monad.  The priority
-- queue will bind the key (a Coord) to a Priority (an Int) and a Partial that
-- was the condition at that time.  Meanwhile, the Cache will store the best
-- partial so far, which may be better or worse than the partial on the priority
-- queue.
findSolutions :: Maze -> IO [(Int, String)]
findSolutions maze = do
    let startXY = findEntrance maze
        keys = map snd $ filter (C.isLower.snd) $ findKeysAndDoors maze
        initialPartial = Partial { _path="@"
                                 , _pathWord=0x00
                                 , _cost=0
                                 , _coord=startXY
                                 , _traversal=H.empty :: H.HashMap Coord Cost
                                 }
        initialTracker = Tracker { _keycode=stringToKeyCode ""
                                 , _partials=[initialPartial]
                                 }
        initialState = SState { _bestCost=maxBound                        -- best cost found so far
                              , _costs=H.empty :: CostsCache              -- cache of xy -> key -> cost
                              , _pqueue=Q.singleton 0x00 0 initialTracker -- priority queue of trackers
                              , _steps=0
                              }
        reader = SReader { _maze=maze, _targetKeyCode=stringToKeyCode keys }
    putStrLn $ "keys=" ++ show keys
    putStrLn $ "initial Tracker" ++ show initialTracker
    putStrLn ""
    (s, w) <- RWS.execRWST processQueue reader initialState
    putStrLn $ "Number of steps was: " ++ show (_steps s)
    pure $ DL.toList w


-- the CostsCache is a Coord -> keycode -> cost
type CostsCache = H.HashMap Coord (H.HashMap KeyCode Int)

data SState = SState { _bestCost :: !Int    -- best cost found so far
                     , _costs :: !CostsCache -- cache of xy -> key -> cost
                     , _pqueue :: !TQueue    -- priority queue of trackers
                     , _steps :: !Int        -- let's count the steps
                     } deriving Show


data SReader = SReader { _maze :: !Maze
                       , _targetKeyCode :: !KeyCode
                       } deriving Show


-- the result of the computation; a set of paths of path length and string
type PQPaths = DL.DList (Int, String)


-- The monad we run processQueue in
type SolutionT m a = RWS.RWST SReader PQPaths SState m a


-- execute the next lowest PQ item, which will call processTracker.  This
-- returns the list of paths in the Writer
processQueue :: SolutionT IO ()
processQueue = do
    SState{_bestCost=bestCost, _pqueue=queue} <- RWS.get
    let next = Q.minView queue
    case next of
        Nothing -> pure ()   -- the paths are in the Writer DLList
        Just (key, cost, tracker, q') -> do
            RWS.modify' $ \ss -> ss {_pqueue=q'}
            if cost > bestCost
              then
                  processQueue
              else do
                  processTracker tracker
                  processQueue


processTracker :: Tracker -> SolutionT IO ()
processTracker Tracker{_partials=ps} | null ps = pure ()   -- no partials, just return
processTracker t@Tracker{_partials=(p@Partial{ _coord=xy
                                             , _cost=pathCost
                                             , _pathWord=key
                                             }:ps)
                        , _keycode=keycode} = do
    SReader{_targetKeyCode=goal} <- RWS.ask
    SState{_bestCost=bestCost, _costs=costMap} <- RWS.get
    isCheaper <- isCheaperAtXY p
    RWS.modify' $ \ss -> ss {_steps=_steps ss +1}
    if pathCost >= bestCost || isCheaper
      then processTracker t {_partials=ps}
      else do
        -- we have the best cost at this point so update the state with the
        -- new cost
        cacheCostAtXY pathCost xy key
        -- now generate candidates
        candidates <- deltaCandidates p
        let (completePaths, remain) =
                partition (\Partial{_pathWord=keysWord}
                  -> keysWord DB..&. goal == goal) candidates
            pathsFound = map (\Partial{_path=path, _cost=cost} -> (cost, path)) completePaths

        -- if we've found a path, then add them to the paths found and update
        -- the minimu cost
        unless (null pathsFound) $ do
            forM_ pathsFound $ RWS.tell . DL.singleton  -- write any new paths to the Writer
            -- update the bestCost
            let minCost = minimum $ bestCost : map fst pathsFound
            RWS.modify' $ \ss -> ss {_bestCost=minCost}

        -- split out the Partials which are this tracker, vs other trackers.
        let (ourPs, otherPs) =
                partition (\Partial{_pathWord=kw} -> kw == key) remain

        -- put the other paths into other trackers
        forM_ otherPs addPartialToTracker

        -- finally we process the rest of this Tracker
        let newPs = addPartialsToPartials ourPs ps
        processTracker t {_partials=newPs}


-- add a partial to an existing or new tracker on the queue
addPartialToTracker :: Partial -> SolutionT IO ()
addPartialToTracker p@Partial{_pathWord=kw, _cost=cost} =
    RWS.modify' $ \ss@SState{_pqueue=queue} -> ss {_pqueue=snd $ Q.alter go kw queue}
  where
    go :: Maybe (Int, Tracker) -> ((), Maybe (Int, Tracker))
    -- create a new tracker for the key if the key doesn't exist
    go Nothing = ((), Just (cost, Tracker {_keycode=kw, _partials=[p]}))
    -- update the existing tracker and cost from the existing tracker.
    go (Just (pcost, t@Tracker{_partials=ps})) =
        ((), Just (minimum [cost,pcost], t {_partials=addPartialToPartials p ps}))


addPartialsToPartials :: [Partial] -> [Partial] -> [Partial]
addPartialsToPartials ps qs = foldr addPartialToPartials qs ps


-- the issue here is that we DON'T want to duplicate paths at locations; we only
-- want to keep the cheapest one and not add a partial that is at the same
-- position/path.  We'll use the pathword and xy to avoid adding the same item
-- and compare by cost.  We assume that ps is already sorted.
addPartialToPartials :: Partial -> [Partial] -> [Partial]
addPartialToPartials p = addPartialToPartials' p []


addPartialToPartials' :: Partial -> [Partial] -> [Partial] -> [Partial]
addPartialToPartials' p ps [] = reverse (p:ps)
addPartialToPartials' p ps (q:qs) =
    let Partial{_pathWord=ppw, _coord=pxy, _cost=pc} = p
        Partial{_pathWord=qpw, _coord=qxy, _cost=qc} = q
        stripSame = filter (\Partial{_pathWord=pw, _coord=xy} -> pw /= ppw || xy /= pxy)
     in case pc `compare` qc of
         LT -> reverse ps ++ (p: stripSame (q:qs))    -- insert the item and end
         GT -> addPartialToPartials' p (q:ps) qs
         EQ -> if ppw == qpw && pxy == qxy   -- are the at the same place
                 then reverse ps ++ (q:qs)  -- discard the items
                 else addPartialToPartials' p (q:ps) qs


showQueue :: TQueue -> IO ()
showQueue q = do
    putStrLn "Queue is:"
    forM_ (Q.toList q) $ \(k,cost,p) ->
        putStrLn $ "At: " ++ show k ++ ",  cost: " ++ show cost ++ ", tracker=" ++ show p
    putStrLn ""


deltaCandidates :: Partial -> SolutionT IO [Partial]
deltaCandidates p@Partial{_coord=xy} = do
    maze <- RWS.asks _maze
    let newDeltas = deltas xy
        chars = map (maze DA.!) newDeltas
        pairs = zip newDeltas chars  -- [(Coord, Char)]
        partials = map (newPartialFromCoordChar p) pairs -- [Maybe Partial]
        mCosts = map (`H.lookup` _traversal p) newDeltas  -- [Maybe cost]
    let keepPs = M.catMaybes $ zipWith keepPartial mCosts partials -- [partial]
    filterM isNotCheaperAtXY keepPs


isCheaperAtXY :: Partial -> SolutionT IO Bool
isCheaperAtXY Partial{_coord=xy, _pathWord=key, _cost=cost} = do
    SState{_costs=costMap} <- RWS.get
    let mKeyToCost = H.lookup xy costMap
        cachedCost = M.fromMaybe maxBound $ mKeyToCost >>= H.lookup key
    pure $ cachedCost < cost


isNotCheaperAtXY :: Partial -> SolutionT IO Bool
isNotCheaperAtXY p = not <$> isCheaperAtXY p


cacheCostAtXY :: Int -> Coord -> KeyCode -> SolutionT IO ()
cacheCostAtXY cost xy key = do
    SState{_costs=costMap} <- RWS.get
    let keyToCost = M.fromMaybe H.empty $ H.lookup xy costMap
    RWS.modify' $ \ss
        -> ss {_costs=H.insert xy (H.insert key cost keyToCost) costMap}


-- This function returns a new Partial from the old partial and the char/xy.
-- If the char is a Door, then the path is updated, but the _pathWord only
-- stores keys; this is so that we don't split at Doors, but we just go through
-- them.
newPartialFromCoordChar :: Partial -> (Coord, Char) -> Maybe Partial
newPartialFromCoordChar p (xy, c)
  | c == '.' || c == '@' = newP
  -- key
  | isKey && hasCode = newP   -- is a key, but we already have it
  | isKey = newPandC          -- is a key, but we don't have it
  -- door
  | isDoor && hasKey && hasDoor = newP  -- is a door, we have the key, but we've seen it
  | isDoor && hasKey = newPandC         -- is a door, we have the key, and we've not seen it
  | otherwise = Nothing                 -- none of the above
    where
        keyCode = charToKeyCode c       -- note, this ignores Doors - only codes keys
        pathWord = _pathWord p
        newCost = _cost p +1
        path = _path p
        isKey = C.isLower c
        isDoor = C.isUpper c
        hasCode = pathWord DB..&. keyCode /= 0
        hasDoor = c `elem` path
        hasKey = pathWord DB..&. charToKeyCode (C.toLower c) /= 0
        newT = H.insert xy (partialToCost p) (_traversal p)
        newP = Just $ p {_coord=xy, _cost=newCost, _traversal=newT}
        newPandC = Just $ p { _coord=xy
                            , _cost=newCost
                            , _path=c : path
                            , _pathWord=pathWord DB..|. keyCode
                            , _traversal=newT}


keepPartial :: Maybe Cost -> Maybe Partial -> Maybe Partial
keepPartial _ Nothing = Nothing
keepPartial Nothing mp = mp
keepPartial (Just c) mp@(Just p) = if c < partialToCost p
                                     then Nothing
                                     else mp

-- utility functions
deltas :: Coord -> [Coord]
deltas xy = map (bothsum xy) [(1,0), (-1,0), (0,1), (0,-1)]

bothsum :: Num a => (a,a) -> (a,a) -> (a,a)
bothsum (a1,b1) (a2,b2) = (a1+a2,b1+b2)

findEntrance :: Maze -> Coord
findEntrance maze = fst $ head $ filter ((=='@').snd) $ DA.assocs maze


findKeysAndDoors :: Maze -> [(Coord,Char)]
findKeysAndDoors maze = filter (C.isAlpha . snd) $ DA.assocs maze

-- test mazes

maze1Txt :: [T.Text]
maze1Txt = [ "#########"
           , "#b.A.@.a#"
           , "#########"
           ]

maze2Txt :: [T.Text]
maze2Txt = [ "########################"
           , "#f.D.E.e.C.b.A.@.a.B.c.#"
           , "######################.#"
           , "#d.....................#"
           , "########################"
           ]

maze3Txt :: [T.Text]
maze3Txt = [ "#################"
           , "#i.G..c...e..H.p#"
           , "########.########"
           , "#j.A..b...f..D.o#"
           , "########@########"
           , "#k.E..a...g..B.n#"
           , "########.########"
           , "#l.F..d...h..C.m#"
           , "#################"
           ]


maze4Txt :: [T.Text]
maze4Txt = [ "########################"
           , "#@..............ac.GI.b#"
           , "###d#e#f################"
           , "###A#B#C################"
           , "###g#h#i################"
           , "########################"
           ]

loadTestMaze :: [T.Text] -> Maze
loadTestMaze = readMap . T.unlines


-- test using maze 4
testIt6 :: IO ()
testIt6 = testItWith maze4Txt


testItWith :: [T.Text] -> IO ()
testItWith txt = do
    putStrLn $ intercalate "\n" $ map T.unpack txt
    let maze = loadTestMaze txt
    solns <- findSolutions maze
    print solns


main35 :: IO ()
main35 = do
    putStrLn "Day 18: Part 1: Many-Worlds Interpretation"
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
    putStrLn "Solutions "
    solns <- findSolutions maze
    print solns
