module Drawing where


import           Data.Function     (on)
import           Data.List         (maximumBy, minimumBy)
import           Data.Maybe        (fromMaybe)
import           Lens.Micro        ((^.), _1)
import           Linear.V2         (V2 (..), _x, _y)

import qualified Data.HashMap.Lazy as H

type Coord = V2 Int
type Drawing = H.HashMap Coord Int

-- drawing utilities

getDimens :: Drawing -> ((Int,Int),(Int,Int))
getDimens m =
    let l = H.toList m
        getX = compare `on` ((^. _x) . fst)
        getY = compare `on` ((^. _y) . fst)
        xmin = minimumBy getX l ^. _1._x
        xmax = maximumBy getX l ^. _1._x
        ymin = minimumBy getY l ^. _1._y
        ymax = maximumBy getY l ^. _1._y
    in ((xmin,xmax),(ymin,ymax))


drawWith :: (Int -> Char) -> Int -> Drawing -> String
drawWith toChar def m =
    let ((xmin,xmax),(ymin,ymax)) = getDimens m
     in if H.null m
          then "{empty map}"
          else unlines [[ toChar (fromMaybe def (H.lookup (V2 x y) m))
                        | x <- [xmin..xmax]]
                        | y <- [ymin..ymax]]
