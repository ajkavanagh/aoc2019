{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DisambiguateRecordFields    #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day18.M35 where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Data.Composition ((.:))

import qualified Data.Array as DA
{-import           Data.Array      (Array, bounds, elems, listArray, (!))-}
import           Data.Array      (Array)
import           Data.Array.MArray (freeze, newListArray, thaw, writeArray, readArray)
import           Data.Array.ST   (STArray)
import           Data.List.Split (chunksOf, keepDelimsL, split, whenElt)
import Data.List (intercalate, nub, nubBy, sort, sortOn, sortBy, groupBy, (\\), isPrefixOf, isInfixOf)
import qualified Data.Char as C
import qualified Data.Maybe as M
import Control.Monad (mapM_, when, forM_, forM, guard)
import qualified Control.Monad.State.Strict as ST
import           Control.Monad.ST  (ST, runST)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as H
import           Data.Hashable (Hashable(..))

import qualified Data.Map as DM
import           FloydWarshall (showShortestPaths, findMinDistances)
import qualified FloydWarshall as FW
import Data.Semigroup

import           Lens.Micro        (each, ix, over, (%~), (&), (.~), (?~), (^.), _2, _1)
-- debuging
import Debug.Trace (trace)


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


{- So we want to have the entrance, doors and keys -}

data Item = Entrance Coord
          | Key Char Coord
          | Door Char Coord
          deriving Eq

instance Ord Item where
    compare (Door _ _) (Key _ _) = LT
    compare (Key _ _) (Door _ _) = GT
    compare (Door c1 _) (Door c2 _) = compare c1 c2
    compare (Key c1 _) (Key c2 _) = compare c1 c2
    compare (Entrance c1) (Entrance c2) = compare c1 c2
    compare (Entrance _) _ = LT
    compare _ (Entrance _) = GT

instance Hashable Item where
    hashWithSalt i n = i + (C.ord . extractChar) n


instance Show Item where
    show (Entrance xy) = "@" ++ show xy
    show (Key c xy) = c : show xy
    {-show (Door c xy) = "D:" ++ C.toUpper c : show xy-}
    show (Door c xy) = c : show xy

makeItem :: Coord -> Char -> Item
makeItem xy '@' = Entrance xy
makeItem xy c
  | C.isUpper c = Door c xy
  | C.isLower c = Key c xy
  | otherwise = error $ error "Can't make a door, key or entrance from " ++ show c


extractChar :: Item -> Char
extractChar (Door c _) = C.toUpper c
extractChar (Key c _) = c
extractChar (Entrance _) = '@'


extractCoord :: Item -> Coord
extractCoord (Entrance xy) = xy
extractCoord (Door _ xy) = xy
extractCoord (Key _ xy) = xy

isDoor :: Item -> Bool
isDoor (Door _ _) = True
isDoor _ = False


hasKey :: Item -> HS.HashSet Char -> Bool
hasKey (Door c _) path = C.toLower c `HS.member` path
hasKey _ _ = False


inPath :: Item -> String -> Bool
inPath i path = extractChar i `elem` path


data Node = Node { _item :: !Item
                 , _cost :: !Int
                 }
                 deriving (Eq)


instance Hashable Node where
    hashWithSalt i n = i + (C.ord . extractChar . (_item :: Node -> Item)) n


instance Show Node where
    show Node{_item=i,_cost=c} = "(" ++ show i ++ "," ++ show c ++")"


data Edge = Edge { _item :: !Item
                 , _nodes :: ![Node]
                 }
                 deriving (Eq, Show)


-- edges are essentially a lookup of an Item (an entrance, door, or key) to
-- A list of Nodes (Item and a cost to get there).  Then we can search Edges
-- to find least cost routes in the whole maze.  The hashmap is strict in that
-- it resolves to WHNF the items and hashset.
type Edges = H.HashMap Item [Node]

buildEdges :: Maze -> Edges
buildEdges maze =
    let startXY = findEntrance maze
        entrance = makeItem startXY '@'
        doorsKeys = map (uncurry makeItem) $ findKeysAndDoors maze
     in H.fromList $ map go (entrance : doorsKeys)
  where
      go :: Item -> (Item, [Node])
      go item =
          let Edge{_item=_, _nodes=ns}  = resolveEdge maze item
           in (item, ns)


-- one issue we have is once we've been to a Node, we then need to alter the
-- graph so that the node can be traversed.  Essentially, we have to take out
-- the Node, and add the costs to items reachable from that Node.
-- An approach is to keep a list/hashset of visited nodes and then use those to
-- be able to 'See' the nodes behind them which getting candidate nodes.
-- So let's try candidates again.
pathsInMazeIO :: Edges
               -> Int
               -> Int
               -> [(Char,Int)]
               -> Item
               -> Int
               -> IO [([(Char,Int)], Int)]
pathsInMazeIO edges count bestCost path item cost =
    ST.evalStateT (pathsInMazeIOST edges count path item cost) bestCost


-- the bestCost so far is held in the state.  We don't do the recursive call
-- if the candidates will exceed the best cost for a path
pathsInMazeIOST :: Edges
                -> Int
                -> [(Char,Int)]
                -> Item
                -> Int
                -> ST.StateT Int IO [([(Char,Int)], Int)] -- hold the bestCost in state
pathsInMazeIOST edges count path item cost | trace ("pathsInMazeIOST: path=" ++ show (reverse (map fst path)) ++ ":" ++ show (extractChar item) ++ " - cost:" ++ show cost) False = undefined
pathsInMazeIOST edges count path item cost
  | length path == count = do
      bestCost <- ST.get
      if cost <= bestCost     -- don't record this path unless it is good
        then do
            ST.lift $ putStrLn $ "Found a path: " ++ show path ++ " which is lower than bestCost (" ++ show bestCost ++ "), so setting bestCost to " ++ show cost ++ " for path:" ++ show path
            ST.put cost          -- new bestCost
            pure [(path, cost)]
        else pure []
  | otherwise = do
      bestCost <- ST.get
      let candidates = candidateNodes edges item (HS.fromList $ map fst path) (bestCost - cost)
      let resolveNext :: Node -> ST.StateT Int IO [([(Char,Int)], Int)]
          resolveNext n@Node{_item=i,_cost=c} =  -- do
              if cost + c <= bestCost
                then pathsInMazeIOST edges count ((extractChar i, c):path) i (cost + c)
                else pure []
      ps <- mapM resolveNext $ candidatesToNodes [] candidates
      pure $ concat ps


data Candidate = Candidate Node
               | ResolveCandidate Edges Node (HS.HashSet Char) Int [Node] Int


instance Show Candidate where
    show (Candidate n) = "Use:" ++ show n
    show (ResolveCandidate _ n path allowedCost nextSofar cost)
      = "Resolve:" ++ show n ++ " at: path: " ++ show path
     ++ ", allowedCost=" ++ show allowedCost
     ++ ", other-nodes:" ++ show nextSofar
     ++ ", current-cost:" ++ show cost


nextNode :: [Item] -> [Candidate] -> (Maybe Node, [Item], [Candidate])
nextNode ps [] = (Nothing, ps, [])
nextNode ps (Candidate n@Node{_item=i}:cs) =
    if i `elem` ps
      then nextNode ps cs
      else (Just n, ps, cs)
nextNode ps (ResolveCandidate edges n@Node{_item=i} path allowedCost nextSofar cost:cs) =
    if i `elem` ps
      then nextNode ps cs
      else
        let rs = elideAndCost' edges n path allowedCost nextSofar cost
            itemsCs = map getItemFromCandidate cs
            rs' = filter (\r -> getItemFromCandidate r `notElem` itemsCs) rs
         in nextNode (i:ps) (rs ++ cs)


nextNodeIO :: [Item] -> [Candidate] -> IO (Maybe Node, [Item], [Candidate])
nextNodeIO ps [] = pure (Nothing, ps, [])
nextNodeIO ps (Candidate n:cs) =  do
    let Node{_item=i} = n
    if i `elem` ps
      then nextNodeIO ps cs
      else pure (Just n, ps, cs)
nextNodeIO ps (c@(ResolveCandidate edges n path allowedCost nextSofar cost):cs) = do
    let Node{_item=i} = n
    if i `elem` ps
      then nextNodeIO ps cs
      else do
        let rs = elideAndCost' edges n path allowedCost nextSofar cost
            itemsCs = map getItemFromCandidate cs
            rs' = filter (\r -> getItemFromCandidate r `notElem` itemsCs) rs
        putStrLn $ "Resolved Candidate: " ++ showC' c ++ " to " ++ intercalate ", " (map showC' rs)
        putStrLn $ "Remaining path is: " ++ intercalate ", " (map showC' cs)
        putStrLn $ "Full path after stripping is: " ++ intercalate ", " (map showC' (rs' ++ cs))
        getLine
        nextNodeIO (i:ps) (rs' ++ cs)


getItemFromCandidate (Candidate Node{_item=i}) = i
getItemFromCandidate (ResolveCandidate _ Node{_item=i} _ _ _ _) = i

showC' (Candidate n) = showN' n
showC' (ResolveCandidate _ n _ _ _ _) = "R:" ++ showN' n

showN' Node{_item=i} = [extractChar i]


candidatesToNodes :: [Item] -> [Candidate] -> [Node]
candidatesToNodes ps [] = []
candidatesToNodes ps cs = let (n,ps',cs') = nextNode ps cs
                       in case n of
                         Nothing -> candidatesToNodes ps' cs'
                         Just n -> n : candidatesToNodes ps' cs'


elideAndCost' :: Edges -> Node -> HS.HashSet Char -> Int-> [Node] -> Int -> [Candidate]
elideAndCost' edges node@Node{_cost=c} path allowedCost sofar cost
  | c + cost >= allowedCost = []
  | otherwise = candidateNodes' edges node path allowedCost (node : sofar) c


candidateNodes :: Edges -> Item -> HS.HashSet Char -> Int -> [Candidate]
candidateNodes edges item path allowedCost =
    let node = Node{_item=item,_cost=0}
     in candidateNodes' edges node path allowedCost [node] 0


candidateNodes' :: Edges
                -> Node              -- the node to look at
                -> HS.HashSet Char   -- where we've already been
                -> Int               -- the allowedCost
                -> [Node]   -- Nodes that have been found in the candidate search
                -> Int               -- the cost so far
                -> [Candidate]   -- the output of Nodes from this point
candidateNodes' edges node path allowedCost sofar cost =
    let Node {_item=item} = node
        all = H.lookupDefault undefined item edges
        costed = addCost all cost
        costed' = filter (\Node{_cost=c} -> c <= allowedCost) costed
        stripped = stripByCost costed' sofar
        doFilter :: Node -> Bool
        doFilter Node{_item=i} = not (isDoor i && not (hasKey i path))
        filtered = filter doFilter stripped
        elideFilter :: Node -> Bool
        elideFilter Node{_item=i} = extractChar i `HS.member` path
        toElide = filter elideFilter filtered
        toKeep = filtered \\ toElide
        -- now add sofar and toKeep, but only keep the cheapest versions
        nextSofar = filterToCheapestPaths $ sofar ++ toKeep
        elided = map (\n -> ResolveCandidate edges n path allowedCost nextSofar cost) toElide
        finalSet = map Candidate toKeep ++ elided
     in finalSet


addCost :: [Node] -> Int -> [Node]
addCost nodes cost = map (\n@Node{_cost=c'} -> n {_cost=cost+c'}) nodes


stripByCost :: [Node] -> [Node] -> [Node]
stripByCost nodes stripFrom = filter (maybeRemove hm) nodes
  where
      hm = H.fromList $ map (\Node{_item=i, _cost=c} -> (i,c)) stripFrom
      maybeRemove :: H.HashMap Item Int -> Node -> Bool
      maybeRemove ns Node{_item=i,_cost=c} = case H.lookup i ns of
          Nothing -> True
          Just c' -> c < c'


estimateAllowedCost :: Edges -> Int
estimateAllowedCost edges = total
  where
      items = H.keys edges
      nodes i = H.lookupDefault undefined i edges
      costs ns = sum $ map (\Node{_cost=c} -> c) ns
      total = sum $ map (costs.nodes) items


-- analyse the maze

findEntrance :: Maze -> Coord
findEntrance maze = fst $ head $ filter ((=='@').snd) $ DA.assocs maze


findKeysAndDoors :: Maze -> [(Coord,Char)]
findKeysAndDoors maze = filter (C.isAlpha . snd) $ DA.assocs maze


resolveEdge :: Maze -> Item -> Edge
resolveEdge maze item =
    let xy = extractCoord item
        paths = filterToCheapestPaths $ pathsFrom maze xy
     in Edge item paths


pathsFrom :: Maze -> Coord -> [Node]
pathsFrom maze = pathsFrom' maze HS.empty 0


pathsFrom' :: Maze -> HS.HashSet Coord -> Int -> Coord -> [Node]
pathsFrom' maze path cost xy = doors ++ keys ++ newPathsFrom
  where
    newCost = cost+1
    deltas = map (bothsum xy) [(1,0), (-1,0), (0,1), (0,-1)]
    newDeltas = filter (not . (`HS.member` path)) deltas
    chars = map (maze DA.!) newDeltas
    pairs = zip newDeltas chars
    newPaths = map fst $ filter ((\c -> c=='.' || c=='@').snd) pairs
    doors = map (\(xy',c) -> Node {_cost=newCost, _item=Door c xy'})
          $ filter (C.isUpper.snd) pairs
    keys = map (\(xy',c) -> Node {_cost=newCost, _item=Key c xy'})
         $ filter (C.isLower.snd) pairs
    newPath = HS.insert xy path
    newPathsFrom = concatMap (pathsFrom' maze newPath newCost) newPaths


filterToCheapestPaths :: [Node] -> [Node]
filterToCheapestPaths ns = hns
  where
      sorterBy :: Node -> Node -> Ordering
      sorterBy Node {_item=i, _cost=c} Node {_item=i2,_cost=c2} =
         case compare i i2 of
            EQ -> compare c c2
            LT -> LT
            GT -> GT
      grouper :: Node -> Node -> Bool
      grouper Node {_item=i1} Node {_item=i2} = i1==i2
      sns = sortBy sorterBy ns
      gns = groupBy grouper sns
      hns = map head gns


filterToCheapestPathsHS :: HS.HashSet Node -> HS.HashSet Node
filterToCheapestPathsHS = HS.fromList . filterToCheapestPaths . HS.toList


bothsum :: Num a => (a,a) -> (a,a) -> (a,a)
bothsum (a1,b1) (a2,b2) = (a1+a2,b1+b2)

-- Try the Floyd-Warshall algorithm
-- Edges is a map of Item -> [Node], where Node is an item & cost
-- Floyd Walsh wants an 2D square array of number of Nodes (or Items), of Ints.
-- We'll run it in ST so we can mutate the array, and it can be unboxed Ints for
-- performance reasons.
-- We need a function for the distance between two KNOWN nodes, and then we fill
-- in the 2D array from that.

costBetween :: Edges -> Item -> Item -> Maybe Int
costBetween edges i1 i2 =
    let nodes = H.lookup i1 edges
        ns = filter (\Node{_item=i} -> i == i2) <$> nodes
      in ns >>= \case
            [] -> Nothing
            Node{_cost=c}:_ -> Just c


-- a Floyd-Warshall array of 0-n, 0-n of vertices
type FWArray = Array (Int,Int) (Maybe Int)
type FWArrayST s = ST s (STArray s (Int,Int) (Maybe Int))


calcFloydWarshallForEdges :: Edges -> FWArray
calcFloydWarshallForEdges edges =
    let items = H.keys edges
        dim = length items -1
        pairs = zip items [0..]
        ij = [(i,j) | i <- pairs, j <- pairs]
        initArray = DA.listArray ((0,0),(dim,dim)) $ repeat Nothing
        kij = [(k,i,j) | k <- [0..dim], i <- [0..dim], j <- [0..dim]]
     in runST $ do
        mArr <- thaw initArray :: FWArrayST s
        -- load the weighings into the array from the edges
        ST.forM_ ij $ \((iItem,i), (jItem,j)) ->
            writeArray mArr (i,j) (costBetween edges iItem jItem)
        -- clear the diagonal
        ST.forM_ [0..dim] $ \i -> writeArray mArr (i,i) (Just 0)
        -- now do the calculation
        ST.forM_ kij $ \(k,i,j) -> do
            dij <- readArray mArr (i,j)
            dik <- readArray mArr (i,k)
            dkj <- readArray mArr (k,j)
            let s = case (dik, dkj) of (Nothing, Nothing) -> Nothing
                                       (Just x, Nothing) -> Nothing
                                       (Nothing, Just y) -> Nothing
                                       (Just x, Just y) -> Just (x + y)
            case (dij, s) of
                (Nothing, Nothing) -> pure ()
                (Just _, Nothing) -> pure ()
                (Nothing, v@(Just _)) -> writeArray mArr (i,j) v
                (Just dij', v@(Just s')) ->
                    when (dij' > s') $ writeArray mArr (i,j) v
        freeze mArr


-- debug help

debugPrintTop :: Maze -> String
debugPrintTop maze = [(maze DA.!) (x,ymin) | x <- [xmin..xmax]]
  where
      ((xmin, ymin), (xmax,_)) = DA.bounds maze

debugPrintBottom :: Maze -> String
debugPrintBottom maze = [(maze DA.!) (x,ymax) | x <- [xmin..xmax]]
  where
      ((xmin, _), (xmax,ymax)) = DA.bounds maze

debugPrintLeft :: Maze -> String
debugPrintLeft maze = [(maze DA.!) (xmin,y) | y <- [ymin..ymax]]
  where
      ((xmin, ymin), (_,ymax)) = DA.bounds maze

debugPrintRight :: Maze -> String
debugPrintRight maze = [(maze DA.!) (xmax,y) | y <- [ymin..ymax]]
  where
      ((_, ymin), (xmax,ymax)) = DA.bounds maze


doorsAndKeys :: [Node] -> String
doorsAndKeys = map (extractChar.(_item :: Node -> Item))


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


testIt :: IO ()
testIt = do
    let maze = loadTestMaze maze4Txt
        edges = buildEdges maze
        entrance = makeItem (findEntrance maze) '@'
    putStrLn $ "Edges: " ++ show edges
    putStrLn "Let's try for some solutions"
    let dk = sort $ map snd $ findKeysAndDoors maze
    putStrLn $ "Need to find " ++ show dk
    putStrLn $ "which is " ++ show (length dk) ++ " keys and doors."
    res <- pathsInMazeIO edges (length dk) (maxBound :: Int) [] entrance 0
    putStrLn $ "Number of solutions " ++ show (length res)
    let paths = sort $ nub $ map fst res
    putStrLn $ "Unique paths " ++ show (length paths)
    let cost = sort $ nub $ map snd res
    putStrLn $ "lowest 5 costs " ++ show (take 5 cost)
    let sres = sortOn snd res
    putStrLn $ "Best 5 paths " ++ show (take 5 sres)
    {-print $ pathsInMaze edges entrance-}
    putStrLn $ "First path " ++ show (head sres)

-- ('h',1),('B',1),('e',37),('b',2),('I',1),('G',35),('i',1),('C',1),('f',9),('g',1),('A',1),('d',15),('c',1),('a',15)
-- try to get to the bottom of why this doesn't cost 'G' and 'e' properly???
testIt2 :: IO ()
testIt2 = do
    let maze = loadTestMaze maze4Txt
        edges = buildEdges maze
        entrance = makeItem (findEntrance maze) '@'
        dk = sort $ map snd $ findKeysAndDoors maze
        foundSoFar = [('i',1),('C',1),('f',9),('g',1),('A',1),('d',15),('c',1),('a',15)]
        (xy,c) = head $ filter ((=='i').snd) $ findKeysAndDoors maze
        iItem = makeItem xy c
    putStrLn "Going to try to debug ('G', 35)"
    res <- pathsInMazeIO edges (length dk) (maxBound :: Int) foundSoFar  iItem 0
    putStrLn $ "Number of solutions " ++ show (length res)
    let paths = sort $ nub $ map fst res
    putStrLn $ "Unique paths " ++ show (length paths)
    let cost = sort $ nub $ map snd res
    putStrLn $ "lowest 5 costs " ++ show (take 5 cost)
    let sres = sortOn snd res
    putStrLn $ "Best 5 paths " ++ show (take 5 sres)
    {-print $ pathsInMaze edges entrance-}
    putStrLn $ "First path " ++ show (head sres)


sumRows :: FWArray -> [Int]
sumRows arr =
    let ((xmin,ymin),(xmax,ymax)) = DA.bounds arr
     in [sum [M.fromMaybe 0 (arr DA.! (x,y)) | x <- [xmin..xmax]]
             | y <- [ymin..ymax]]

sumCols :: FWArray -> [Int]
sumCols arr =
    let ((xmin,ymin),(xmax,ymax)) = DA.bounds arr
     in [sum [M.fromMaybe 0 (arr DA.! (x,y)) | y <- [ymin..ymax]]
             | x <- [xmin..xmax]]


minRows :: FWArray -> [Int]
minRows arr =
    let ((xmin,ymin),(xmax,ymax)) = DA.bounds arr
        minimum' [] = 0
        minimum' xs = minimum xs
     in [minimum' $ filter (>0) [M.fromMaybe 0 (arr DA.! (x,y)) | x <- [xmin..xmax]]
             | y <- [ymin..ymax]]


drawWith :: (Int -> String) -> Int -> FWArray -> String
drawWith toString def m =
    let ((xmin,ymin),(xmax,ymax)) = DA.bounds m
     in unlines [unwords [ toString (M.fromMaybe def (m DA.! (x,y)))
                         | x <- [xmin..xmax]]
                | y <- [ymin..ymax]]


showMe :: Int -> String
showMe i = if i < 0
             then " ."
             else let s = reverse (show i)
                   in reverse (take 2 (s ++ repeat ' '))


testIt3 :: IO ()
testIt3 = do
    let maze = loadTestMaze maze4Txt
        edges = buildEdges maze
        arr = calcFloydWarshallForEdges edges
    putStrLn $ drawWith showMe (-1) arr
    putStrLn "Sum of rows"
    print $ sumRows arr
    putStrLn "Sum of cols"
    print $ sumCols arr
    putStrLn "Minimum of rows"
    print $ minRows arr


-- use the Floyd-Warshall module (rosetta code) to find the shortest lengths
testIt4 :: IO ()
testIt4 = do
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
        edges = buildEdges maze
        items = H.keys edges
        all = [((i,j), costBetween edges i j) | i <- items, j <- items]
        g = DM.fromList $ map (\(c,x) -> (c,M.fromMaybe 0 x)) $ filter (M.isJust.snd) all
        ds = findMinDistances items (Sum <$> g)
    {-showShortestPaths items (Sum <$> g)-}
    mapM_ print $ take 10 $ DM.toList ds
    putStrLn $ "Total options: " ++ show (length $ DM.toList ds)
    let entrance = makeItem (findEntrance maze) '@'
        keys = ['a'..'z']
        pairs = zip keys (drop 1 keys)
        kds = findKeysAndDoors maze
        getItem c = head $ filter ((==c).snd) kds
        makeItem' c = uncurry makeItem $ getItem c
        first = (entrance, makeItem' 'a')
        rest = map (\(i,j) -> (makeItem' i, makeItem' j)) pairs
        paths = map (`DM.lookup` ds) $ first : rest
    mapM_ print paths



testIt5 :: IO ()
testIt5 = do
    let maze = loadTestMaze maze4Txt
        edges = buildEdges maze
        items = H.keys edges
        {-all = DM.fromList [((i,j), costBetween edges i j) | i <- items, j <- items]-}
        all = [((i,j), costBetween edges i j) | i <- items, j <- items]
        g = DM.fromList $ map (\(c,x) -> (c,M.fromMaybe 0 x)) $ filter (M.isJust.snd) all
    showShortestPaths items (Sum <$> g)

--
-- another set of ways to handle the paths using the FloydWarshall module

type FWMap = DM.Map (Char,Char) (FW.Shortest String (Sum Int))


shortestPaths :: Edges -> (FWMap, String)
shortestPaths edges =
    let items = H.keys edges
        {-chars = filter (\c -> C.isLower c || c == '@') $ map extractChar items-}
        chars = map extractChar items
        all = [((extractChar i,extractChar j), costBetween edges i j) | i <- items, j <- items]
        g = DM.fromList $ map (\(c,x) -> (c,M.fromMaybe 0 x)) $ filter (M.isJust.snd) all
        ds = findMinDistances chars (Sum <$> g)
     in (ds,chars)


shortestFrom :: (FWMap, String) -> Char -> [(Int, String)]
shortestFrom (ds, all) from =
    let keys = map (from,) $ filter (/=from) all
     in sortOn fst $ filter ((/=0).fst) $ map (extractPath ds) keys


extractPath :: FWMap -> (Char, Char) -> (Int, String)
extractPath fwmap k =
    case DM.lookup k fwmap of
        Nothing -> (0, "")
        Just r -> (getSum $ FW.distance r, head $ FW.path r)



findShortestPath :: FWMap -> Char -> (Int, String)
findShortestPath fwmap from = undefined


findShortestPathIO :: FWMap -> Char -> IO (Int, String)
findShortestPathIO fwmap from = undefined


data PathState = PathState { _bestCost :: Int
                           , _pathsSearched :: [String]
                           , _lookups :: [(String, Char)]
                           } deriving Show


-- find the shortest paths from a particular point
findShortestPathsIO :: FWMap  -- the map of shortest paths
                    -> String -- the current path traversed
                    -> String -- the remaining keys to find (i.e. not in the path)
                    -> Int    -- the bestCost found so far
                    -> Int    -- the current cost of the path so far
                    -> IO [(Int, String)]
findShortestPathsIO fwmap path remain bestCost cost =
    ST.evalStateT (findShortestPathsIOST fwmap path remain cost) PathState { _bestCost=bestCost
                                                                           , _pathsSearched=[]
                                                                           , _lookups=[]
                                                                           }


findShortestPathsIOST :: FWMap  -- a map of shortest paths
                      -> String -- the current path traversed
                      -> String -- the remaining keys to find (i.e. not in the path)
                      -> Int    -- the current cost of the path so far
                      -> ST.StateT PathState IO [(Int, String)] -- hold the bestCost in state
findShortestPathsIOST _ path "" cost = do
    bestCost <- ST.gets _bestCost
    if cost < bestCost
      then do
          ST.lift $ putStrLn $ "Fount a new path at cost: " ++ show cost ++ ", path=" ++ show path
          ST.modify' $ \pstate -> pstate {_bestCost=cost}
          pure [(cost, path)]
      else do
          ST.lift $ putStrLn $ "Found a more expensive path: " ++ show path ++ ", cost=" ++ show cost
          pure []
findShortestPathsIOST fwmap path remain cost = do
    bestCost <- ST.gets _bestCost
    lookups <- ST.gets _lookups
    {-let sRemain = sort remain-}
    {-ST.lift $ putStrLn $ "params: " ++ show path ++ ", " ++ show remain ++ ", cost:" ++ show cost ++ ", bestCost:" ++ show bestCost-}
    let from = last path
    {-if (remain, from) `elem` lookups-}
    if False
      then pure []
      else do
        let ss = shortestFrom (fwmap, remain) from
            {-paths = filter (not.null.snd) $ map (second tail) $ filterToPossiblePaths path ss-}
            paths = filter (not.null.snd) $ map (second tail) $ keepPathsWithKeys path ss
            -- now take out paths that end where we've been
            trimmed = filter (\(_,p) -> last p `notElem` path) paths
            remainingCost = bestCost - cost
            inCostPaths = filter ((<remainingCost).fst) trimmed
            {-uniquePrefixes = keepUniquePrefixes inCostPaths-}
            {-strippedCycles = stripTraversedSegments path uniquePrefixes-}
        ST.modify' $ \pstate -> pstate {_lookups = (remain, from):lookups}
        {-ST.lift $ putStrLn $ "Remaing cost " ++ show remainingCost-}
        {-ST.lift $ putStrLn $ "Going from " ++ show from-}
        {-ST.lift $ putStrLn $ "paths: " ++ (intercalate ", " (map show ss))-}
        {-ST.lift $ putStrLn $ "inCostPaths: " ++ (intercalate ", " (map show inCostPaths))-}
        {-ST.lift $ putStrLn $ "uniquePrefixes: " ++ (intercalate ", " (map show uniquePrefixes))-}
        {-ST.lift $ putStrLn $ "strippedCycles: " ++ (intercalate ", " (map show strippedCycles))-}
        {-ST.lift getLine-}
        ps <- forM inCostPaths $ \(c, p) -> do
            prevPaths <- ST.gets _pathsSearched
            let r = stripKeysFrom remain p
                newPath = path ++ p
                {-sortedP = sort newPath-}
            {-if sortedP `elem` prevPaths-}
            if newPath `elem` prevPaths
                then pure []
                else do
                    ST.modify' $ \pstate -> pstate {_pathsSearched = newPath:prevPaths}
                    findShortestPathsIOST fwmap (path ++ p) r (cost + c)
        pure $ concat ps

first = over _1
second = over _2


data PathState2 = PathState2 { _bestCostSoFar :: !Int
                             , _paths :: ![(Int, String)]
                             , _prevPaths :: ![String]
                             , _allKeys :: !String
                             } deriving Show


priorityQueueNextIO :: FWMap -> String -> String -> Int -> IO (Int, [(Int, String)])
priorityQueueNextIO fwmap allKeys path bestCost= do
    let state = PathState2 { _bestCostSoFar=bestCost
                           , _paths=[]
                           , _prevPaths=[]
                           , _allKeys=allKeys }
    (bc, s) <- ST.runStateT (priorityQueueNextIOST fwmap [(0, path)]) state
    pure (bc, _paths s)



-- this gives the wrong answer of 7482.  I will have to try this again, but just
-- using the just the edge graph, rather than the fwmap
priorityQueueNextIOST :: FWMap  -- a map of shortest paths
                      -> [(Int, String)] -- the priority queue of paths
                      -> ST.StateT PathState2 IO Int
priorityQueueNextIOST _ [] = ST.gets _bestCostSoFar
priorityQueueNextIOST fwmap ((cost, path):qs) = do
    ST.lift $ putStrLn $ "params: cost=" ++ show cost ++ ", path=" ++ show path -- ++ ", remaining: " ++ show (length qs)
    {-ST.lift $ putStrLn $ intercalate ", " (map show qs)-}
    bestCost <- ST.gets _bestCostSoFar
    {-ST.lift $ putStrLn $ "bestcost so far: " ++ show bestCost-}
    {-ST.lift getLine-}
    if cost >= bestCost
      then priorityQueueNextIOST fwmap qs
      else do
        allKeys <- ST.gets _allKeys
        {-let remain = stripKeysFrom path allKeys-}
        let remain = stripKeysFrom allKeys path
        {-ST.lift $ putStrLn $ "remain = " ++ show remain-}
        if null remain
          then do
              let bestCostSoFar = minimum [bestCost, cost]
              ST.modify' $ \pstate -> pstate {_paths=(cost,path): _paths pstate
                                             ,_bestCostSoFar=bestCostSoFar
                                             }
              ST.lift $ putStrLn $ "Found a path: " ++ show path ++ ", cost: " ++ show cost
              ST.lift getLine
              priorityQueueNextIOST fwmap qs
          else do
              let from = last path
                  ss = shortestFrom (fwmap, remain) from
                  paths = filter (not.null.snd) $ map (second tail) $ keepPathsWithKeys path ss
                  -- now take out paths that end where we've been
                  trimmed = filter (\(_,p) -> last p `notElem` path) paths
                  remainingCost = bestCost - cost
                  inCostPaths = filter ((<remainingCost).fst) trimmed
                  expandPaths = map (second (path++)) inCostPaths
                  costedPaths = map (first (cost+)) expandPaths
                  nextQs = insertIntoPath qs costedPaths
                  nextQs' = stripBeenThere path nextQs
              priorityQueueNextIOST fwmap nextQs'


insertIntoPath :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
insertIntoPath qs ns =
    let ns' = sortOn fst ns
     in insertIntoPath' compCostThenLenKeys [] qs ns'


compCostThenLenKeys :: (Int, String) -> (Int, String) -> Ordering
compCostThenLenKeys (c1, p1) (c2, p2) =
    case compare c1 c2 of
        EQ -> let k1 = keysIn p1
                  k2 = keysIn p2
               in case compare (length p2) (length p1) of
                   EQ -> compare (length k2) (length k1)
                   x -> x
        x -> x
  where
      keysIn = filter C.isLower


compPathLenThenCost :: (Int, String) -> (Int, String) -> Ordering
compPathLenThenCost (c1, p1) (c2, p2) =
    case compare (length p2) (length p1) of
        EQ -> compare c1 c2
        x -> x


insertIntoPath' :: (a -> a -> Ordering) -> [a] -> [a] -> [a] -> [a]
insertIntoPath' f ss [] [] = reverse ss
insertIntoPath' f ss bs [] = reverse ss ++ bs
insertIntoPath' f ss [] cs = reverse ss ++ cs
insertIntoPath' f ss bs@(b:bs') cs@(c:cs') =
    case f b c of
        EQ -> insertIntoPath' f (b:ss) bs' cs'
        LT -> insertIntoPath' f (b:ss) bs' cs
        GT -> insertIntoPath' f (c:ss) bs cs'


stripBeenThere :: String -> [(Int, String)] -> [(Int, String)]
stripBeenThere path options =
    let sPath = sort path
        beenThere :: (Int, String) -> Bool
        beenThere (_, s) = sort s == sPath
     in filter (not.beenThere) options


stripKeysFrom :: String -> String -> String
stripKeysFrom remain candidate =
    let keys = filter C.isLower candidate
     in filter (`notElem` keys) remain


splitAtNoKeyDoor :: String -> String -> [String]
splitAtNoKeyDoor path = split (keepDelimsL $ whenElt noKey)
  where
      noKey :: Char -> Bool
      noKey c = C.isUpper c && C.toLower c `notElem` path


hasNoKey :: String -> String -> Bool
hasNoKey path = any noKey
  where
      noKey :: Char -> Bool
      noKey c = C.isUpper c && C.toLower c `notElem` path


-- the first param is the path so far, the 2nd is the potential path.  This
-- function then returns the length of path that can be traversed (added to the)
-- path so far, due to keys, etc.
longestPathWith :: String -> String -> String
longestPathWith = head .: splitAtNoKeyDoor
-- (.:) (c -> d) -> (a -> b -> c) -> (a -> b -> d)


filterToPossiblePaths :: String -> [(Int, String)] -> [(Int, String)]
filterToPossiblePaths path options =
    let possibles = map (\p -> p & _2 %~ longestPathWith path) options
     in nubBy (\a b -> snd a == snd b) possibles


keepPathsWithKeys :: String -> [(Int, String)] -> [(Int, String)]
keepPathsWithKeys path = filter (not . hasNoKey path . snd)


keepUniquePrefixes :: [(Int, String)] -> [(Int, String)]
keepUniquePrefixes paths =
    let sorted = sortOn snd paths
     in nubBy (\a b -> snd a `isPrefixOf` snd b) sorted


stripTraversedSegments :: String -> [(Int, String)] -> [(Int, String)]
stripTraversedSegments path = filter (not . alreadyDone)
  where
      alreadyDone :: (Int, String) -> Bool
      alreadyDone (_, p) | length p > 1 = p `isInfixOf` path
      alreadyDone _ = False


-- we want a path from x-Y that includes the key 'y' .. i.e. we need a path
-- that has a key for the door.  We also want it to be the shortest path.
shortestPathWithKey :: FWMap -> Char -> Char -> (Int, String)
shortestPathWithKey fwmap from to = undefined




-- test using maze 4
testIt6 :: IO ()
testIt6 = do
    let maze = loadTestMaze maze4Txt
        edges = buildEdges maze
        (ds, all) = shortestPaths edges
    print all
    {-print ds-}
    forM_ all $ \c ->
        when (C.isLower c || c == '@') $ do
            putStrLn $ "From " ++ show c
            putStrLn $ intercalate ", " (map show $ shortestFrom (ds, all) c)
            putStrLn "And now the adjusted ones"
            putStrLn $ intercalate ", " (map show $ filterToPossiblePaths "" $ shortestFrom (ds, all) c)
    putStrLn "And now lets try a calculation"
    let keys = filter C.isLower all
        maxAllowedCost = estimateAllowedCost edges
    {-res <- findShortestPathsIO ds "@" keys maxAllowedCost 0-}
    res <- priorityQueueNextIO ds keys "@" maxAllowedCost
    print res


-- test using the input
testIt7 :: IO ()
testIt7 = do
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
        edges = buildEdges maze
        (ds, all) = shortestPaths edges
    print all
    {-print ds-}
    forM_ all $ \c ->
        when (C.isLower c || c == '@') $ do
            putStrLn $ "From " ++ show c
            putStrLn $ intercalate ", " (map show $ shortestFrom (ds, all) c)
            putStrLn "And now the adjusted ones"
            putStrLn $ intercalate ", " (map show $ filterToPossiblePaths "" $ shortestFrom (ds, all) c)
    putStrLn "And now lets try a calculation"
    let keys = filter C.isLower all
        maxAllowedCost = estimateAllowedCost edges
    {-res <- findShortestPathsIO ds "@" keys maxAllowedCost 0-}
    res <- priorityQueueNextIO ds keys "@" maxAllowedCost
    print res


debugMain35 :: IO ()
debugMain35 = do
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
        edges = buildEdges maze
        path = reverse "cCeEkKoOthzamyxAMYXqQlgLTHGiIZujJUdwDWr"
    putStrLn "here"
    print $ findKeysAndDoors maze
    let (xy,c) = head $ filter ((=='r').snd) $ findKeysAndDoors maze
        item = makeItem xy c
        maxAllowedCost = estimateAllowedCost edges
        allowedCost = maxAllowedCost - 16588
        candidates = candidateNodes edges item (HS.fromList path) allowedCost
    print candidates
    let go [] [] = pure []
        go ps cs = do
            (n, ps', cs') <- nextNodeIO ps cs
            putStrLn $ show n ++ " <=> " ++ show ps' ++ " :: " ++ show cs'
            putStrLn $ "Press enter for next."
            getLine
            go ps' cs'
    res <- go [] candidates
    pure ()




main35 :: IO ()
main35 = do
    putStrLn "Day 18: Part 1: Many-Worlds Interpretation"
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
    let edges = buildEdges maze
        entrance = makeItem (findEntrance maze) '@'
    let dk = sort $ map snd $ findKeysAndDoors maze
    putStrLn $ "Need to find " ++ show dk
    putStrLn $ "which is " ++ show (length dk) ++ " keys and doors."
    let maxAllowedCost = estimateAllowedCost edges
    res <- pathsInMazeIO edges (length dk) maxAllowedCost [] entrance 0
    putStrLn $ "Number of solutions " ++ show (length res)
    let paths = sort $ nub $ map fst res
    putStrLn $ "Unique paths " ++ show (length paths)
    let cost = sort $ nub $ map snd res
    putStrLn $ "lowest 5 costs " ++ show (take 5 cost)
    let sres = sortOn snd res
    putStrLn $ "Best 5 paths " ++ show (take 5 sres)
    {-print $ pathsInMaze edges entrance-}
    putStrLn $ "First path " ++ show (head sres)
