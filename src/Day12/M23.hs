{-# LANGUAGE OverloadedStrings #-}

module Day12.M23 where



import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Data.Function          (on)
import           Data.List              (nub, sortBy, foldr1, iterate')

import           Control.Applicative    ((<|>))
import           Control.Monad          (void)
import           Text.Parsec            (oneOf, parse, parseTest, parserFail,
                                         try)
import           Text.Parsec.Char       (char, digit, spaces, string)
import           Text.Parsec.Combinator (endBy, endBy1, eof, many1, manyTill)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String     (Parser)


moonsFile = "files/12/moons.txt"


loadMoonText :: IO T.Text
loadMoonText = TIO.readFile moonsFile


data Moon = Moon { _pos :: (Int,Int,Int)
                 , _v   :: (Int,Int,Int)
                 } deriving Show

makeMoon :: [Axis] -> Moon
makeMoon as = let vs = map snd $ sortBy (compare `on` fst) $ map go as
               in Moon { _pos=(head vs, vs !! 1, vs !! 2)
                       , _v = (0,0,0)
                       }
  where
      go :: Axis -> (Int,Int)
      go (Axis xyz v) = case xyz of
          Xaxis -> (0, v)
          Yaxis -> (1, v)
          Zaxis -> (2, v)


loadMoons :: T.Text -> Either ParseError [Moon]
loadMoons txt = do
    rawMoons <- mapM parseMoon $ lines $ T.unpack txt
    pure $ map makeMoon rawMoons


-- read and parse the line:
-- <x=4, y=0, z=1>
-- into (4,0,1) as a tuple

data XYZ = Xaxis | Yaxis | Zaxis deriving (Show, Eq)
data Axis = Axis XYZ Int deriving (Show, Eq)


parseMoon :: String -> Either ParseError [Axis]
parseMoon = regularParse matchLine


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


matchLine :: Parser [Axis]
matchLine = do
    spaces
    void $ char '<'
    spaces
    a1 <- matchSection
    matchInterSection
    a2 <- matchSection
    matchInterSection
    a3 <- matchSection
    void $ char '>'
    let check = validateAxis [a1,a2,a3]
    if check
      then pure [a1,a2,a3]
      else parserFail "line doesn't contain x,y,z uniquely"


validateAxis :: [Axis] -> Bool
validateAxis as = length (nub xs) == 3
  where
      xs = map (\(Axis x _) -> x) as


matchSection :: Parser Axis
matchSection = do
    c <- oneOf "xyz"
    void $ char '='
    spaces
    Axis (toXYZ c) <$> matchNumber


matchInterSection :: Parser ()
matchInterSection = do
    void $ char ','
    spaces
    pure ()


toXYZ :: Char -> XYZ
toXYZ 'x' = Xaxis
toXYZ 'y' = Yaxis
toXYZ 'z' = Zaxis


matchNumber :: Parser Int
matchNumber = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Int
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit


-- Now gravity operations

tfmap :: (a -> b) -> (a,a,a) -> (b,b,b)
tfmap f (x,y,z) = (f x,f y,f z)

tfapply2 :: (a -> b ->c ) -> (a,a,a) -> (b,b,b) -> (c,c,c)
tfapply2 f a b = tfmap (uncurry f) $ pairTriple a b


pairTriple :: (a,b,c) -> (d,e,f) -> ((a,d), (b,e), (c,f))
pairTriple (a,b,c) (d,e,f) = ((a,d), (b,e), (c,f))


unPairTriple :: ((a,d),(b,e),(c,f)) -> ((a,b,c),(d,e,f))
unPairTriple ((a,d),(b,e),(c,f)) = ((a,b,c),(d,e,f))


applyGravityAxis :: Integral a => a -> a -> a
applyGravityAxis x y = case x `compare` y of
    LT -> 1
    EQ -> 0
    GT -> -1


applyGravity :: Integral a => (a,a,a) -> (a,a,a) -> (a,a,a)
applyGravity = tfapply2 applyGravityAxis


applyGravityMoons :: [Moon] -> Moon -> [(Int,Int,Int)]
applyGravityMoons ms m =
    map (applyGravity (_pos m) . _pos) ms

sumGravityDeltas :: [(Int,Int,Int)] -> (Int,Int,Int)
sumGravityDeltas = foldr1 (tfapply2 (+))


newMoonGravityDelta :: [Moon] -> Moon -> (Int,Int,Int)
newMoonGravityDelta ms m = sumGravityDeltas $ applyGravityMoons ms m


newGravityDeltas :: [Moon] -> [(Int,Int,Int)]
newGravityDeltas ms = map (newMoonGravityDelta ms) ms


updateVelocityWithDeltas :: [Moon] -> [(Int,Int,Int)] -> [Moon]
updateVelocityWithDeltas = zipWith (curry go)
  where
      go :: (Moon, (Int,Int,Int)) -> Moon
      go (m, delta) = m { _v=tfapply2 (+) (_v m) delta }


updateMoonPositions :: [Moon] -> [Moon]
updateMoonPositions = map go
  where
      go :: Moon -> Moon
      go m = m { _pos=tfapply2 (+) (_pos m) (_v m) }


stepMoons :: [Moon] -> [Moon]
stepMoons ms = let deltas = newGravityDeltas ms
                in updateMoonPositions $ updateVelocityWithDeltas ms deltas


moonEnergy :: Moon -> Int
moonEnergy m = product $ map (sum3tuple . tfmap abs) [_pos m, _v m]


sum3tuple :: Integral a => (a,a,a) -> a
sum3tuple (x,y,z) = x+y+z


moonsEnergy :: [Moon] -> Int
moonsEnergy = sum . map moonEnergy


doSteps :: Int -> [Moon] -> [Moon]
doSteps n ms = head $ drop n $ iterate' stepMoons ms

-- test sets
testSet1 :: [T.Text]
testSet1 = [ "<x=-1, y=0, z=2>"
           , "<x=2, y=-10, z=-7>"
           , "<x=4, y=-8, z=8>"
           , "<x=3, y=5, z=-1>"
           ]


testSet2 :: [T.Text]
testSet2 = [ "<x=-8, y=-10, z=0>"
           , "<x=5, y=5, z=10>"
           , "<x=2, y=-7, z=3>"
           , "<x=9, y=-8, z=-3>"
           ]


loadTestSet :: [T.Text] -> [Moon]
loadTestSet ts = case loadMoons $ T.unlines ts of
    Left ex -> error $ show ex
    Right ms -> ms


main23 :: IO ()
main23 = do
    putStrLn "Day12, Part 1: N-body problem"
    txt <- loadMoonText
    let ms = loadTestSet (T.lines txt)
        ms' = doSteps 1000 ms
        e = moonsEnergy ms'
    putStrLn $ "Energy of system after 1000 steps is: " <> show e

