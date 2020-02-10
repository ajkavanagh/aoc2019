{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}

module Day18.M35 where

import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Composition           ((.:))

{-import           Data.Array      (Array, bounds, elems, listArray, (!))-}
import           Control.Monad              (forM, forM_, guard, mapM_, when, foldM)
import           Control.Monad.Primitive    (PrimState)
import           Control.Monad.ST           (ST, runST)
import qualified Control.Monad.State.Strict as ST

import           Data.Array                 (Array)
import qualified Data.Array                 as DA
import           Data.Array.MArray          (freeze, newListArray, readArray,
                                             thaw, writeArray)
import           Data.Array.ST              (STArray, newListArray)
import           Data.Array.IO              (IOArray)
import qualified Data.Array.IO              as DAIO

import qualified Data.Char                  as C
import           Data.List                  (groupBy, intercalate, isInfixOf,
                                             isPrefixOf, nub, nubBy, sort,
                                             sortBy, sortOn, (\\), partition)
import           Data.List.Split            (chunksOf, keepDelimsL, split,
                                             whenElt)
import qualified Data.Maybe                 as M

import           Data.Hashable              (Hashable (..))
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as HS

{-import           Data.HashPSQ               (HashPSQ)-}
{-import qualified Data.HashPSQ               as Q-}
import qualified Data.IntPSQ                as Q

import qualified Data.Map                   as DM
import           Data.Semigroup
import           FloydWarshall              (findMinDistances,
                                             showShortestPaths)
import qualified FloydWarshall              as FW

import           Lens.Micro                 (both, each, ix, over, (%~), (&),
                                             (.~), (?~), (^.), _1, _2)
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

data Partial = Partial { _path  :: String  -- path is reversed
                       , _cost  :: Int
                       , _numKeys :: Int    -- to tell when we've finished
                       , _coord :: Coord
                       , _traversal :: H.HashMap Coord Cost
                       } deriving (Eq)


instance Show Partial where
    show Partial{_path=p, _cost=c, _numKeys=nk, _coord=xy, _traversal=t} =
        "P(" ++ show p ++ ", c:" ++ show c ++ ", nk: " ++ show nk ++ ", at: " ++ show xy ++ ", len: " ++ show (H.size t) ++")"



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

-- finally, we want to cache everywhere we've been, so we'll have an array of
-- partials; it's a Maybe as Nothing means that nobody has been there.
type Cache = Array Coord (Maybe Partial)
type MCache s = STArray (PrimState (ST s)) Coord (Maybe Partial)
type IOCache = IOArray Coord (Maybe Partial)


-- The priority Queue is a @Data.HashPSQ k p v@ where k is Coord, p is Int
-- (cost) and v is a Partial
{-type Queue = HashPSQ Coord Cost Partial-}
type Queue = Q.IntPSQ Cost Partial

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
                                 , _cost=0
                                 , _numKeys=0
                                 , _coord=startXY
                                 , _traversal=H.empty :: H.HashMap Coord Cost
                                 }
    putStrLn $ "keys=" ++ show keys
    putStrLn $ "initial partial" ++ show initialPartial
    putStrLn ""
    {-pure $ runST $ do-}
         {-mcache <- newListArray (DA.bounds maze) $ repeat Nothing-}
         {-processQueue mcache maze (length keys) (Q.singleton startXY (partialToCost initialPartial) initialPartial) []-}
    {-mcache <- newListArray (DA.bounds maze) $ repeat Nothing-}
    {-writeArray mcache startXY (Just initialPartial)-}
    {-processQueueIO mcache maze (length keys) (Q.singleton startXY (partialToCost initialPartial) initialPartial) []-}
    ST.evalStateT (processQueueSTIO maze (length keys) (Q.singleton 0 (partialToCost initialPartial) initialPartial) []) (PQState {_index=1, _bestPaths=H.empty})


-- We'll loop until the priority queue is empty -- this should result in the
-- possible paths found in the result
{-processQueue :: MCache s -> Maze -> Int -> Queue -> [(Int, String)] -> ST s [(Int, String)]-}
{-processQueue _ _ keysGoal queue pathsFound | trace ("\nprocessQueue: queue: " ++ show queue) False = undefined-}
{-processQueue mcache maze keysGoal queue pathsFound = do-}
    {-let next = Q.minView queue-}
    {-case next of-}
        {-Nothing -> pure pathsFound-}
        {-Just (xy, cost, partial, q') -> do-}
            {-mPAtXY <- readArray mcache xy  -- Maybe Partial-}
            {--- if pAtXY < partial then we just skip this queue item, as we've-}
            {--- already got a better path at that location.-}
            {-let skip = case mPAtXY of Nothing -> False-}
                                      {-Just pAtXY -> pAtXY < partial-}
            {-if skip-}
              {-then processQueue mcache maze keysGoal q' pathsFound-}
              {-else do-}
                  {-candidates <- deltaCandidates mcache maze partial-}
                  {-let (completePaths, remain) = partition (\Partial{_numKeys=nk} -> nk == keysGoal) candidates-}
                      {-pathsFound' = pathsFound ++ map (\Partial{_path=path, _cost=cost} -> (cost, path)) completePaths-}
                      {-q'' = foldr (\p@Partial{_coord=xy} q -> Q.insert xy (partialToCost p) p q) q' remain-}
                  {-processQueue mcache maze keysGoal q'' pathsFound'-}


{-processQueueIO :: IOCache -> Maze -> Int -> Queue -> [(Int, String)] -> IO [(Int, String)]-}
{-processQueueIO mcache maze keysGoal queue pathsFound = do-}
    {-let next = Q.minView queue-}
    {-case next of-}
        {-Nothing -> pure pathsFound-}
        {-Just (xy, cost, partial, q') -> do-}
            {-putStrLn $ "Processing: " ++ show (_coord partial) ++ ": " ++ show partial-}
            {-mPAtXY <- readArray mcache xy  -- Maybe Partial-}
            {--- if pAtXY < partial then we just skip this queue item, as we've-}
            {--- already got a better path at that location.-}
            {-putStrLn $ "Againt cache: " ++ show mPAtXY-}
            {-let skip = case mPAtXY of Nothing -> False-}
                                      {-Just pAtXY -> pAtXY < partial-}
            {-if skip-}
              {-then  do-}
                  {-putStrLn "Skipping - as cache was less than partial"-}
                  {-getLine-}
                  {-processQueueIO mcache maze keysGoal q' pathsFound-}
              {-else do-}
                  {-writeArray mcache xy (Just partial)-}
                  {-candidates <- deltaCandidatesIO mcache maze partial-}
                  {-putStrLn $ "Candidates are: " ++ intercalate ", " (map show candidates)-}
                  {-let (completePaths, remain) = partition (\Partial{_numKeys=nk} -> nk == keysGoal) candidates-}
                      {-pathsFound' = pathsFound ++ map (\Partial{_path=path, _cost=cost} -> (cost, path)) completePaths-}
                  {-putStrLn $ "adding to Queue: " ++ intercalate ", " (map show remain)-}
                  {-let q'' = foldr (\p@Partial{_coord=xy} q -> Q.insert xy (partialToCost p) p q) q' remain-}
                  {-showQueue q''-}
                  {-{-getLine-}-}
                  {-processQueueIO mcache maze keysGoal q'' pathsFound'-}


{-processQueue2IO :: Maze -> Int -> Queue -> [(Int, String)] -> IO [(Int, String)]-}
{-processQueue2IO maze keysGoal queue pathsFound = do-}
    {-let next = Q.minView queue-}
    {-case next of-}
        {-Nothing -> pure pathsFound-}
        {-Just (xy, cost, partial, q') -> do-}
            {-putStrLn $ "Processing: " ++ show (_coord partial) ++ ": " ++ show partial-}
            {-putStrLn $ "Cost here is: " ++ show (partialToCost partial)-}
            {-candidates <- deltaCandidates2IO maze partial-}
            {-putStrLn $ "Candidates are: " ++ intercalate ", " (map show candidates)-}
            {-let (completePaths, remain) = partition (\Partial{_numKeys=nk} -> nk == keysGoal) candidates-}
                {-pathsFound' = pathsFound ++ map (\Partial{_path=path, _cost=cost} -> (cost, path)) completePaths-}
            {-putStrLn $ "adding to Queue: " ++ intercalate ", " (map show remain)-}
            {--- add in the where the partial is to the new remains-}
            {-let remain' = map (\p@Partial{_traversal=t} -> p {_traversal=H.insert xy (partialToCost partial) t}) remain-}
            {-let q'' = foldr (\p@Partial{_coord=xy} q -> Q.insert xy (partialToCost p) p q) q' remain'-}
            {-showQueue q''-}
            {-{-getLine-}-}
            {-processQueue2IO maze keysGoal q'' pathsFound'-}

data PQState = PQState { _index :: Int
                       , _bestPaths :: H.HashMap String Int
                       } deriving Show


processQueueSTIO:: Maze -> Int -> Queue -> [(Int, String)] -> ST.StateT PQState IO [(Int, String)]
processQueueSTIO maze keysGoal queue pathsFound = do
    let next = Q.minView queue
    case next of
        Nothing -> pure pathsFound
        Just (i, cost, partial@Partial{_coord=xy}, q') -> do
            ST.lift $ putStrLn $ "Processing: item:" ++ show i ++ " at " ++ show (_coord partial) ++ ": " ++ show partial
            ST.lift $ putStrLn $ "Cost here is: " ++ show (partialToCost partial)
            candidates <- deltaCandidatesSTIO maze partial
            ST.lift $ putStrLn $ "Candidates are: " ++ intercalate ", " (map show candidates)
            let (completePaths, remain) = partition (\Partial{_numKeys=nk} -> nk == keysGoal) candidates
                pathsFound' = pathsFound ++ map (\Partial{_path=path, _cost=cost} -> (cost, path)) completePaths
            ST.lift $ putStrLn $ "adding to Queue: " ++ intercalate ", " (map show remain)
            -- add in the where the partial is to the new remains
            let remain' = map (\p@Partial{_traversal=t} -> p {_traversal=H.insert xy (partialToCost partial) t}) remain
            {-let q'' = foldr (\p@Partial{_coord=xy} q -> Q.insert xy (partialToCost p) p q) q' remain'-}
            q'' <- foldM (\q p -> do
                i <- ST.gets _index
                ST.modify' $ \pq ->  pq { _index=i+1 }
                pure $ Q.insert (i+1) (partialToCost p) p q
                         ) q' remain'

            ST.lift $ showQueue q''
            {-ST.lift getLine-}
            processQueueSTIO maze keysGoal q'' pathsFound'

showQueue :: Queue -> IO ()
showQueue q = do
    putStrLn "Queue is:"
    forM_ (Q.toList q) $ \(k,Cost (len, cost),Partial{_path=path}) ->
        putStrLn $ "At: " ++ show k ++ ", Cost: " ++ show len ++ " path, cost: " ++ show cost ++ ", path=" ++ show path
    putStrLn ""


-- based on the current partial path, look at the deltas around and return a
-- set of partials that are viable to move towards.
{-deltaCandidates :: MCache s -> Maze -> Partial -> ST s [Partial]-}
{-deltaCandidates _ _ p | trace ("deltaCandidates: partial=" ++ show p) False = undefined-}
{-deltaCandidates mcache maze p = do-}
    {-let newDeltas = deltas $ _coord p-}
        {-chars = map (maze DA.!) newDeltas-}
        {-pairs = zip newDeltas chars  -- [(Coord, Char)]-}
        {-partials = map (newPartialFromCoordChar p) pairs -- [Maybe Partial]-}
    {-cPs <- mapM (readArray mcache) newDeltas -- [Maybe Partial]-}
    {-let keepPs = M.catMaybes $ zipWith keepMinimumPartial cPs partials -- [Partial]-}
    {--- write the lowest searches back to the cache.-}
    {-forM_ keepPs $ \p'@Partial{_coord=xy} ->-}
         {-writeArray mcache xy (Just p')-}
    {-pure keepPs-}


{-deltaCandidatesIO :: IOCache -> Maze -> Partial -> ST.StateT Int IO [Partial]-}
{-deltaCandidatesIO mcache maze p = do-}
    {-let newDeltas = deltas $ _coord p-}
        {-chars = map (maze DA.!) newDeltas-}
        {-pairs = zip newDeltas chars  -- [(Coord, Char)]-}
        {-partials = map (newPartialFromCoordChar p) pairs -- [Maybe Partial]-}
    {-cPs <- mapM (readArray mcache) newDeltas -- [Maybe Partial]-}
    {-{-putStrLn "print candidates?"-}-}
    {-{-x <- getLine-}-}
    {-let x = "y"-}
    {-when (x == "y") $ do-}
        {-putStrLn $ "partials: " ++ intercalate ", " (map show partials)-}
        {-putStrLn $ "cached: " ++ intercalate ", " (map show cPs)-}
    {-let keepPs = M.catMaybes $ zipWith keepMinimumPartial cPs partials -- [Partial]-}
    {-when (x == "y") $ do-}
        {-ST.lift $ putStrLn $ "What's left: " ++ intercalate ", " (map show keepPs)-}
    {--- write the lowest searches back to the cache.-}
    {-{-forM_ keepPs $ \p'@Partial{_coord=xy} ->-}-}
         {-{-writeArray mcache xy (Just p')-}-}
    {-pure keepPs-}


{-deltaCandidates2IO :: Maze -> Partial -> IO [Partial]-}
{-deltaCandidates2IO maze p = do-}
    {-let newDeltas = deltas $ _coord p-}
        {-chars = map (maze DA.!) newDeltas-}
        {-pairs = zip newDeltas chars  -- [(Coord, Char)]-}
        {-partials = map (newPartialFromCoordChar p) pairs -- [Maybe Partial]-}
        {-mCosts = map (`H.lookup` _traversal p) newDeltas  -- [Maybe cost]-}
    {-{-cPs <- mapM (readArray mcache) newDeltas -- [Maybe Partial]-}-}
    {-{-putStrLn "print candidates?"-}-}
    {-{-x <- getLine-}-}
    {-let x = "y"-}
    {-when (x == "y") $ do-}
        {-putStrLn $ "partials     : " ++ intercalate ", " (map show partials)-}
        {-putStrLn $ "partial costs: " ++ intercalate ", " (map (show . fmap partialToCost) partials)-}
        {-putStrLn $ "costs        : " ++ intercalate ", " (map show mCosts)-}
    {-let keepPs = M.catMaybes $ zipWith keepPartial mCosts partials -- [Partial]-}
    {-when (x == "y") $-}
        {-putStrLn $ "What's left: " ++ intercalate ", " (map show keepPs)-}
    {--- write the lowest searches back to the cache.-}
    {-{-forM_ keepPs $ \p'@Partial{_coord=xy} ->-}-}
         {-{-writeArray mcache xy (Just p')-}-}
    {-pure keepPs-}
deltaCandidatesSTIO :: Maze -> Partial -> ST.StateT PQState IO [Partial]
deltaCandidatesSTIO maze p@Partial{_path=path} = do
    let newDeltas = deltas $ _coord p
        chars = map (maze DA.!) newDeltas
        pairs = zip newDeltas chars  -- [(Coord, Char)]
        partials = map (newPartialFromCoordChar p) pairs -- [Maybe Partial]
        mCosts = map (`H.lookup` _traversal p) newDeltas  -- [Maybe cost]
    {-cPs <- mapM (readArray mcache) newDeltas -- [Maybe Partial]-}
    {-putStrLn "print candidates?"-}
    {-x <- getLine-}
    let x = "y"
    when (x == "y") $ do
        ST.lift $ putStrLn $ "partials     : " ++ intercalate ", " (map show partials)
        ST.lift $ putStrLn $ "partial costs: " ++ intercalate ", " (map (show . fmap partialToCost) partials)
        ST.lift $ putStrLn $ "costs        : " ++ intercalate ", " (map show mCosts)
    let keepPs = M.catMaybes $ zipWith keepPartial mCosts partials -- [Partial]
    when (x == "y") $
        ST.lift $ putStrLn $ "What's left: " ++ intercalate ", " (map show keepPs)
    -- write the lowest searches back to the cache.
    {-forM_ keepPs $ \p'@Partial{_coord=xy} ->-}
         {-writeArray mcache xy (Just p')-}
    -- finally lose paths which we already have a better cost for at the point
    -- we get the path
    m <- ST.gets _bestPaths
    let (nps, eps) = partition (\Partial{_path=p'} -> p' /= path) keepPs  -- ([Partial],[Partial])
        bps = map ((`H.lookup` m)._path) nps  -- [Maybe Int] -- of the new paths
        (nps', m') = foldr (\(pa@Partial{_path=p, _cost=c}, mi) (bs, mbps) -> case mi of
            Nothing -> (pa:bs, H.insert p c mbps)
            Just c' -> if c' > c then (pa:bs, H.insert p c mbps)
                                 else (bs, mbps)) ([], m) $ zip nps bps
    ST.modify' $ \pq -> pq {_bestPaths=m'}
    pure $ eps ++ nps'


newPartialFromCoordChar :: Partial -> (Coord, Char) -> Maybe Partial
newPartialFromCoordChar p (xy, c)
  | c == '.' || c == '@' = Just $ p {_coord=xy, _cost=newCost}
  -- key
  | C.isLower c = if c `elem` path
                    then Just $ p {_coord=xy, _cost=newCost}
                    else Just $ p {_coord=xy, _cost=newCost, _path=c : path, _numKeys=_numKeys p +1}
  -- door
  | C.isUpper c = if C.toLower c `elem` path
                    then if c `elem` path
                           then Just $ p {_coord=xy, _cost=newCost}
                           else Just $ p {_coord=xy, _cost=newCost, _path=c : path}
                    else Nothing
  | otherwise = Nothing
    where
        newCost = _cost p +1
        path = _path p


-- keep the minimum partial, but if the left one is less then just return
-- Nothing.  The reason is to avoid returning where we've already been
keepMinimumPartial :: Maybe Partial -> Maybe Partial -> Maybe Partial
keepMinimumPartial Nothing Nothing = Nothing
keepMinimumPartial mp@(Just p) Nothing = Nothing
keepMinimumPartial Nothing mp@(Just p) = mp
keepMinimumPartial (Just p1) (Just p2) = if p1 < p2
                                           then Nothing
                                           else Just p2

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
    putStrLn "First path "
