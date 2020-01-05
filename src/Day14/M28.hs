{-# LANGUAGE OverloadedStrings #-}

module Day14.M28 where



import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Data.Function          (on)
import           Data.List              (foldr1, iterate', nub, sortBy)
import           Data.Tuple.Extra       (first)

import           Control.Applicative    ((<|>))
import           Control.Monad          (void)
import           Text.Parsec            (oneOf, parse, parseTest, parserFail,
                                         try)
import           Text.Parsec.Char       (char, digit, spaces, string, upper)
import           Text.Parsec.Combinator (endBy, endBy1, eof, many1, manyTill,
                                         sepBy, sepBy1)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String     (Parser)

import qualified Data.HashMap.Lazy      as H


moonsFile = "files/14/reactions.txt"


loadReactionsText :: IO T.Text
loadReactionsText = TIO.readFile moonsFile

-- parse the reactions into a data structure

type Quantity = (Int, String)
type Reaction = H.HashMap String (Int, [Quantity])

-- A map of the Chemical to (Number left, number made)
type Reactor = H.HashMap String (Int, Int)


loadReactions :: T.Text -> Either ParseError Reaction
loadReactions txt = do
    rawReactions <- mapM parseReaction $ lines $ T.unpack txt
    pure $ H.unions rawReactions


loadTestSet :: [T.Text] -> Either ParseError Reaction
loadTestSet = loadReactions . T.unlines


parseReaction :: String -> Either ParseError Reaction
parseReaction = regularParse matchLine


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


matchLine :: Parser Reaction
matchLine = do
    qs <- matchQuantities
    spaces
    string "=>"
    spaces
    (v, k) <- matchQuantity
    pure $ H.singleton k (v, qs)


matchQuantities :: Parser [Quantity]
matchQuantities = commaSep1 matchQuantity


matchQuantity :: Parser Quantity
matchQuantity = do
    n <- matchNumber
    spaces
    s <- many1 upper
    pure (n, s)


matchNumber :: Parser Int
matchNumber = rd <$> number
    where rd     = read :: String -> Int
          number = many1 digit


commaSep1 :: Parser a -> Parser [a]
commaSep1 = (`sepBy1` comma)


comma :: Parser Char
comma = lexeme $ char ','


lexeme :: Parser a -> Parser a
lexeme p = p <* spaces


-- Now the resolution algorithms

{-
   We want to go from a single chemical and then resolve recursively the
   elements until we can's resolve them any further.  Essentially, we work
   through the list and recursively resolve the elements.

   e.g. for 10 A, 5 B, 3 C => 9 F

   we need the lcm of 9 and 10,5,3 to workout how big the reaction is (i.e.
   how many F we need to pull off the that reaction.  we end up with with
   resolved reactions of x F => y ORE

   9 ORE => 2 A
   8 ORE => 3 B
   7 ORE => 5 C
   3 A, 4 B => 1 AB
   5 B, 7 C => 1 BC
   4 C, 1 A => 1 CA
   2 AB, 3 BC, 4 CA => 1 FUEL

   Resolve AB = (1, [(3,A),(4,B)])

   A = (2, [(9,ORE)])  ... but we need 3
   B = (3, [(8,ORE)])  ,,, but we need 4

   Resolve 1 FUEL = (1, [(2,AB), (3,BC), (4, CA)])

The above list of reactions requires 165 ORE to produce 1 FUEL:

    Consume 45 ORE to produce 10 A.
    Consume 64 ORE to produce 24 B.
    Consume 56 ORE to produce 40 C.
    Consume 6 A, 8 B to produce 2 AB.
    Consume 15 B, 21 C to produce 3 BC.
    Consume 16 C, 4 A to produce 4 CA.
    Consume 2 AB, 3 BC, 4 CA to produce 1 FUEL.

In reverse, this is:

   2AB => 6A + 4B
        6A => 3 x 2A => 18 ORE (6A consumed, 0A spare)
        4B => 2 x 3B => 16 ORE (3B consumed, 1B spare)
   3BC => 15B + 21C
        15B => 5 x 3B => 24 ORE (15B consumed, still 1B spare)
        21C => 5 x 5C => 35 ORE (21C consumed, 4 spare)
    4CA => 16C + 4A
        16C => 3 x 5C => 21 ORE (16C consumed, 15 added, 3C remaining)
        4A => 2 x 2A => 18 ORE (4A consumed)

    Total is 18+16+24+21+18  -- which is wrong!

    We can do it using a Reactor which does the reactions.
-}


-- Reactor


newReactor :: Reactor
newReactor = H.empty


consume :: Quantity -> Reactor -> Reaction -> Reactor
consume q@(n,chem) r rt =
    let (avail, _) = H.lookupDefault (0,0) chem r
     in if avail >= n
          then H.insertWith useUp chem (n,0) r
          else let r' = makeAtLeast q r rt
                in H.insertWith useUp chem (n,0) r'


applyBoth :: (a -> a -> a) -> (b -> b -> b) -> (a,b) -> (a,b) -> (a,b)
applyBoth f g (a1,b1) (a2,b2) = (f a1 a2, g b1 b2)


useUp :: Num a => (a,b) -> (a,b) -> (a,b)
useUp = applyBoth (flip (-)) (flip const)


-- make at least (in the reactor) n of chem
-- This may require recursing into the reactants to also make those
makeAtLeast :: Quantity -> Reactor -> Reaction -> Reactor
makeAtLeast q@(n,chem) r rt =
    let (avail, made) = H.lookupDefault (0,0) chem r
        (toMake, reactants) = H.lookupDefault (1,[]) chem rt
        multiple = determineMultiple avail toMake n
     in if avail >= n
          then r
          else let r' = foldr (consumeForMe multiple) r reactants
                   addIn = multiple * toMake
                in H.insertWith addMade chem (addIn,addIn) r'
  where
    consumeForMe :: Int -> Quantity -> Reactor -> Reactor
    consumeForMe n qq rr = let qq' = applyBoth (*) const qq (n, "")
                            in consume qq' rr rt


addMade :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addMade = applyBoth (+) (+)


-- Satisfy "x * toMake + avail >= wanted"
determineMultiple :: Int -> Int -> Int -> Int
determineMultiple _ 0 _ = 1
determineMultiple avail toMake wanted =
    let multiple = (wanted - avail) `div` toMake
     in if multiple * toMake + avail >= wanted
          then multiple
          else multiple +1


-- finally for a Reactor, how much s was made
chemMade :: String -> Reactor -> Int
chemMade chem r = snd $ H.lookupDefault (0,0) chem r

-- for part 2 we have to find the fuel that just fits into 1 Trillian ORE

oneT :: Int
oneT = 1000000000000

findUpperPower2 :: (Int -> Int) -> Int -> Int
findUpperPower2 f target = fst $ head $ dropWhile ((<target).snd) $ iterate' go (1, 0)
  where
      go :: ((Int,Int) -> (Int,Int))
      go (input, lastResult) = let next = input * 2 in (next, f next)


binarySearch :: (Int -> Ordering) -> Int -> Int -> Int
binarySearch test l u
  | l == u = l
  | l > u = binarySearch test u l
  | l == u-1 = u
  | otherwise = let m = minimum [((u - l) `div` 2) + l, u]
                 in case test m of
                     EQ -> m
                     LT -> binarySearch test m u
                     GT -> binarySearch test l m


-- Note the -1 is because the checkIt function finds the one just above oneT,
-- not below it
findFuelForOneT :: Reaction -> Int
findFuelForOneT rts = result -1
  where
    upper = findUpperPower2 runIt oneT
    result = binarySearch checkIt 1 upper
    runIt :: Int -> Int
    runIt n = chemMade "ORE" $ consume (n, "FUEL") newReactor rts
    checkIt :: Int -> Ordering
    checkIt n = runIt n `compare` oneT


-- test sets for testing the algorithms

testIt :: [T.Text] -> Int
testIt ts = let (Right rts) = loadTestSet ts
                in chemMade "ORE" $ consume (1, "FUEL") newReactor rts


loadIt :: [T.Text] -> Reaction
loadIt ts = let (Right rts) = loadTestSet ts in rts

testSet1 :: [T.Text]
testSet1 = [ "10 ORE => 10 A"
           , "1 ORE => 1 B"
           , "7 A, 1 B => 1 C"
           , "7 A, 1 C => 1 D"
           , "7 A, 1 D => 1 E"
           , "7 A, 1 E => 1 FUEL"
           ]

testSet2 :: [T.Text]
testSet2 = [ "9 ORE => 2 A"
           , "8 ORE => 3 B"
           , "7 ORE => 5 C"
           , "3 A, 4 B => 1 AB"
           , "5 B, 7 C => 1 BC"
           , "4 C, 1 A => 1 CA"
           , "2 AB, 3 BC, 4 CA => 1 FUEL"
           ]

testSet3 :: [T.Text]
testSet3 = [ "157 ORE => 5 NZVS"
           , "165 ORE => 6 DCFZ"
           , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
           , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
           , "179 ORE => 7 PSHF"
           , "177 ORE => 5 HKGWZ"
           , "7 DCFZ, 7 PSHF => 2 XJWVT"
           , "165 ORE => 2 GPVTF"
           , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
           ]

testSet4 :: [T.Text]
testSet4 = [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
           , "17 NVRVD, 3 JNWZP => 8 VPVL"
           , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
           , "22 VJHF, 37 MNCFX => 5 FWMGM"
           , "139 ORE => 4 NVRVD"
           , "144 ORE => 7 JNWZP"
           , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
           , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
           , "145 ORE => 6 MNCFX"
           , "1 NVRVD => 8 CXFTF"
           , "1 VJHF, 6 MNCFX => 4 RFSQX"
           , "176 ORE => 6 VJHF"
           ]

testSet5 :: [T.Text]
testSet5 = [ "171 ORE => 8 CNZTR"
           , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
           , "114 ORE => 4 BHXH"
           , "14 VRPVC => 6 BMBT"
           , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
           , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
           , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
           , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
           , "5 BMBT => 4 WPTQ"
           , "189 ORE => 9 KTJDG"
           , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
           , "12 VRPVC, 27 CNZTR => 2 XDBXC"
           , "15 KTJDG, 12 BHXH => 5 XCVML"
           , "3 BHXH, 2 VRPVC => 7 MZWV"
           , "121 ORE => 7 VRPVC"
           , "7 XCVML => 6 RJRHP"
           , "5 BHXH, 4 VRPVC => 5 LTCX"
           ]


main28 :: IO ()
main28 = do
    putStrLn "Day14, Part 2: Space Stoichiometry - fuel for 1_000_000_000_000 ORE"
    txt <- loadReactionsText
    let mrts = loadReactions txt
    case mrts of
        Left ex -> print ex
        Right rts -> do
            let res = findFuelForOneT rts
            putStrLn $ "The amount of FUEL for 1T ORE is: " ++ show res

