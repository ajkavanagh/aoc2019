{-# LANGUAGE OverloadedStrings #-}

module Day03.M06
    ( main06
    ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as    T
import qualified Data.List as    L
import           Data.Maybe      (isJust)

wiresFile :: String
wiresFile = "files/03/wires.txt"

data Direction = North | South | West | East deriving (Eq, Show)
data Orientation = Horiz | Vert deriving (Eq, Show)


data Segment = Segment { line    :: (Int,Int,Int,Int)
                       , end     :: (Int,Int)
                       , dir     :: Orientation
                       , slen    :: Int
                       , pointer :: Direction
                       }

instance Show Segment where
    show s = "<" ++ show sx ++ "," ++ show sy ++ " -> "
          ++ show ex ++ "," ++ show ey ++ " : ("
          ++ show zx ++ "," ++ show zy ++ ") " ++ show (dir s)
          ++ " ~ " ++ show (pointer s)
          ++ " / " ++ show (slen s)
          ++ " >"
      where (sx,sy,ex,ey) = line s
            (zx,zy) = end s


parseSegment :: T.Text -> (Direction, Int)
parseSegment t =
    let c = T.head t
        a = (read $ T.unpack (T.tail t)) :: Int
    in  case c of
        'U' -> (North, a)
        'D' -> (South, a)
        'L' -> (West, a)
        'R' -> (East, a)
        _ -> error "Didn't understand direction"


splitWire :: T.Text -> [T.Text]
splitWire = T.split (==',')


makeSegment :: (Int,Int) -> (Direction, Int) -> Segment
makeSegment (x,y) (d,l) = case d of
    North -> Segment { line=(x,y+1,x,y+l), end=(x,y+l), dir=Vert, slen=l, pointer=North }
    South -> Segment { line=(x,y-l,x,y-1), end=(x,y-l), dir=Vert, slen=l, pointer=South }
    West  -> Segment { line=(x-l,y,x-1,y), end=(x-l,y), dir=Horiz, slen=l, pointer=West }
    East  -> Segment { line=(x+1,y,x+l,y), end=(x+l,y), dir=Horiz, slen=l, pointer=East }


mapWire :: [T.Text] -> [Segment]
mapWire ts = snd res
  where res = L.foldl' go ((0,0), []) ts
        go :: ((Int,Int), [Segment]) -> T.Text -> ((Int,Int), [Segment])
        go (xy,ss) t = let ms = makeSegment xy (parseSegment t)
                        in (end ms, ss ++ [ms])


makePath :: [Segment] -> [(Int, Segment)]
makePath ss = fst res
  where
      res = L.foldl' go ([(0, head ss)], 0) $ tail ss
      go :: ([(Int, Segment)], Int) -> Segment -> ([(Int, Segment)], Int)
      go (p, sp) s = (p ++ [(t, s)], t)
        where t =sp + slen s

makePath2 :: [Segment] -> [(Int, Segment)]
makePath2 ss = out
  where
      res = L.foldl' go ([(0, head ss)], 0, slen (head ss)) $ tail ss
      go :: ([(Int, Segment)], Int, Int) -> Segment -> ([(Int, Segment)], Int, Int)
      go (ps, p, n) s = (ps ++ [(t, s)], t, n')
        where t = p + n
              n' = slen s
      (out, _, _) = res


intersections :: [Segment] -> [Segment] -> [(Int,Int)]
intersections xs ys = map unJust $ filter isJust [segmentIntersection x y | x <- xs, y <- ys]


unJust :: Maybe a -> a
unJust Nothing = error "Not Nothings at this point please!"
unJust (Just x) = x


pathIntersections :: [(Int, Segment)] -> [(Int, Segment)] -> [(Int, (Int, Int))]
pathIntersections xs ys = map unJust $ filter isJust [pathSegmentIntersection x y | x <- xs, y <- ys]


pathSegmentIntersection :: (Int, Segment) -> (Int, Segment) -> Maybe (Int, (Int,Int))
pathSegmentIntersection (pa, a) (pb, b) =
    maybeCrosses >>= \xy ->
        Just (pa + pb + stepsIn xy a + stepsIn xy b, xy)
  where
      maybeCrosses = segmentIntersection a b


stepsIn :: (Int,Int) -> Segment -> Int
stepsIn (x,y) s = case pointer s of
    North -> y - ys +1  -- e.g (x, 5) above (x, 3) = 2
    South -> ye - y +1   -- e.g.(x, 5) below (x, 7)
    East  -> x - xs +1
    West  -> xe - x +1
  where
      (xs,ys,xe,ye) = line s
      _dir          = pointer s



segmentIntersection :: Segment -> Segment -> Maybe (Int,Int)
segmentIntersection a b = res
  where (xs1,ys1,xe1,ye1) = line a
        (xs2,ys2,xe2,ye2) = line b
        res = case (dir a, dir b) of
            (Horiz, Horiz) | ys1 /= ys2                 -> Nothing
                           | (xe1 < xs2) || (xe2 < xs1) -> Nothing
                           | otherwise                  -> Just (closest0 xs1 xe1 xs2 xe2, ys1)
            (Vert, Vert)   | xs1 /= xs2                 -> Nothing
                           | (ye1 < ys2) || (ye2 < ys1) -> Nothing
                           | otherwise                  -> Just (xs1, closest0 ys1 ye1 ys2 ye2)
            (Horiz, Vert)  -> if xs1 <= xs2 && ys1 >= ys2 && xe1 >= xe2 && ye1 <= ye2
                                then Just (xs2,ye1)
                                else Nothing
            (Vert, Horiz)  -> if xs2 <= xs1 && ys2 >= ys1 && xe2 >= xe1 && ye2 <= ye1
                                then Just (xs1,ye2)
                                else Nothing

closest0 :: Int -> Int -> Int -> Int -> Int
closest0 as ae bs be = head $ L.sortOn abs candidates
  where
    candidates
      | as >= bs && ae <= be = [as, ae]
      | bs >= as && be <= ae = [bs, be]
      | as < bs = [bs, ae]
      | otherwise = [as, be]


manhattens :: [(Int,Int)] -> [Int]
manhattens = map (uncurry pluser)
  where pluser x y = abs x + abs y

nearest :: T.Text -> T.Text -> Int
nearest w1 w2 = minimum (manhattens $ intersections (mapWire (splitWire w1))
                                                    (mapWire (splitWire w2)))


nearest2 :: T.Text -> T.Text -> Int
nearest2 w1 w2 = minimum (map fst (pathIntersections (makePath2 (mapWire (splitWire w1)))
                                                     (makePath2 (mapWire (splitWire w2)))))

main06 :: IO ()
main06 = do
    putStrLn $ "Hi, using: " ++ wiresFile
    wireText <- TIO.readFile wiresFile
    let lines = T.lines wireText
    print $ makeSegment (0,0) (East,4)
    print $ mapWire $ splitWire "U2,R3"
    let wire1 = "R8,U5,L5,D3"
        wire2 = "U7,R6,D4,L4"
    print $ mapWire $ splitWire wire1
    print $ makePath2 $ mapWire $ splitWire wire1
    print $ mapWire $ splitWire wire2
    print $ makePath2 $ mapWire $ splitWire wire2
    print $ pathIntersections (makePath2 (mapWire (splitWire wire1)))
                              (makePath2 (mapWire (splitWire wire2)))

    let wire3 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    let wire4 = "U62,R66,U55,R34,D71,R55,D58,R83"
    print $ pathIntersections (makePath2 (mapWire (splitWire wire3)))
                              (makePath2 (mapWire (splitWire wire4)))
    let wire5 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    let wire6 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    print $ pathIntersections (makePath2 (mapWire (splitWire wire5)))
                              (makePath2 (mapWire (splitWire wire6)))

    putStrLn "and finally"
    print $ nearest2 (head lines) (lines !! 1)



