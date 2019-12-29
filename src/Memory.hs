module Memory where

-- Memory module that stores Nums of some kind
-- provides 'fetch' and 'store' instructions.
-- allows an allocation from a List of Nums
-- automatically increases size when trying to store/get at a new location.
-- This re-allocates the entire store.


import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import qualified Data.List       as L
import qualified Data.List.Split as S


data Memory a = Memory { memBlocks    :: Vector (Maybe (Vector a))
                       , memBlockSize :: Int
                       , empty        :: a
                       } deriving Show


type MemoryInt = Memory Int

newtype MemoryException = InvalidMemoryLocation Int

instance Show MemoryException where
    show (InvalidMemoryLocation p) = "Invalid Memory position: " ++ show p


initMemory :: Int -> Int -> a -> Memory a
initMemory size num d = Memory { memBlocks=V.replicate num Nothing
                               , memBlockSize=size
                               , empty=d
                               }

defSize :: Int
defSize = 30
defNum = 500


defaultInitMemory :: a -> Memory a
defaultInitMemory = initMemory defSize defNum


-- initialise the memory from a list
loadMemoryFromList :: Int -> Int -> a -> [a] -> Memory a
loadMemoryFromList size num d vs =
    let blocks = S.chunksOf size (vs ++ L.replicate size d)
        lenVs = length vs
        remain = lenVs `mod` size
        initNumBlocks = lenVs `div` size + (if remain > 0 then 1 else 0)
        nothingNumBlocks = num - initNumBlocks
        loadBlocks = map (Just . V.fromList) $ take initNumBlocks blocks
     in Memory { memBlocks=V.fromList $ loadBlocks ++ L.replicate nothingNumBlocks Nothing
               , memBlockSize=size
               , empty=d
               }


defaultLoadMemoryFromList :: a -> [a] -> Memory a
defaultLoadMemoryFromList = loadMemoryFromList defSize defNum


fetch :: Memory a -> Int -> Either MemoryException a
fetch mem pos = let blockSize = memBlockSize mem
                    blockNum = pos `div` blockSize
                    maxBlocks = V.length (memBlocks mem)
                 in if blockNum >= maxBlocks
                      then Left $ InvalidMemoryLocation pos
                      else let block = memBlocks mem V.! blockNum
                            in case block of
                                Nothing -> Right $ empty mem
                                Just blk -> Right $ blk V.! (pos `mod` blockSize)


store :: Memory a -> Int -> a -> Either MemoryException (Memory a)
store mem pos v = let blockSize = memBlockSize mem
                      blockNum = pos `div` blockSize
                      blockI   = pos `mod` blockSize
                      maxBlocks = V.length (memBlocks mem)
                   in if blockNum >= maxBlocks
                        then Left $ InvalidMemoryLocation pos
                        else let block = memBlocks mem V.! blockNum
                              in Right $ case block of
                                  Nothing -> let newBlock = V.replicate blockSize (empty mem) V.// [(blockI, v)]
                                              in mem { memBlocks=memBlocks mem V.// [(blockNum, Just newBlock)] }
                                  Just blk -> mem { memBlocks=memBlocks mem V.// [(blockNum, Just (blk V.// [(blockI, v)]))] }


bounds :: Memory a -> (Int, Int)
bounds m = (0, V.length (memBlocks m) * memBlockSize m -1)
