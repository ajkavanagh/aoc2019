{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}

module Day18.M35 where

import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Composition           ((.:))

import qualified Data.Array                 as DA
{-import           Data.Array      (Array, bounds, elems, listArray, (!))-}
import           Control.Monad              (forM, forM_, guard, mapM_, when)
import           Control.Monad.ST           (ST, runST)
import qualified Control.Monad.State.Strict as ST
import           Data.Array                 (Array)
import           Data.Array.MArray          (freeze, newListArray, readArray,
                                             thaw, writeArray)
import           Data.Array.ST              (STArray)
import qualified Data.Char                  as C
import           Data.List                  (groupBy, intercalate, isInfixOf,
                                             isPrefixOf, nub, nubBy, sort,
                                             sortBy, sortOn, (\\))
import           Data.List.Split            (chunksOf, keepDelimsL, split,
                                             whenElt)
import qualified Data.Maybe                 as M

import           Data.Hashable              (Hashable (..))
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as HS

import qualified Data.Map                   as DM
import           Data.Semigroup
import           FloydWarshall              (findMinDistances,
                                             showShortestPaths)
import qualified FloydWarshall              as FW

import           Lens.Micro                 (each, ix, over, (%~), (&), (.~),
                                             (?~), (^.), _1, _2)
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


{- So we want to have the entrance, doors and keys -}

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





main35 :: IO ()
main35 = do
    putStrLn "Day 18: Part 1: Many-Worlds Interpretation"
    mazeTxt <- loadMaze
    let maze = readMap mazeTxt
    putStrLn "First path "
