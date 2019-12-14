{-# LANGUAGE OverloadedStrings #-}

module Day04.M08
    ( main08
    , parsePassword
    , validatePassword
    , validatedPasswordsBetween
    ) where

import qualified Data.List as    L
import           Data.Char       (digitToInt)


parsePassword :: String -> [Int]
parsePassword = reverse . map digitToInt


showPassword :: Show a => [a] -> String
showPassword = concatMap show . reverse


validatePassword :: Ord a => [a] -> Bool
validatePassword xs = alwaysIncreases xs && hasOnlyTwo xs

-- passwords are backwards, remember!
alwaysIncreases :: Ord a => [a] -> Bool
alwaysIncreases [] = error "No empty arrays!"
alwaysIncreases (x:xs) = snd $ L.foldl' go (x,True) xs
  where
      go :: Ord a => (a, Bool) -> a -> (a, Bool)
      go (l, f) n = (n, f && n <= l)


hasOnlyTwo :: Eq a => [a] -> Bool
hasOnlyTwo = elem 2 . map length . L.group


incPassword :: (Ord a, Num a) => [a] -> Maybe [a]
incPassword = incP 0

incP :: (Ord a, Num a) =>  Int -> [a] -> Maybe [a]
incP _ [] = Nothing
incP c (r:rs) | r == 9 = incP (c+1) rs
incP c (r:rs)          = let r' = r + 1 in Just (L.replicate (c+1) r' ++ rs)


passwordsBetween :: String -> String -> [[Int]]
passwordsBetween s1 s2 =
    let p1 = parsePassword s1
        p2 = parsePassword s2
    in  p1 : takeWhile (passwordNotGT p2) (L.unfoldr inc p1)
   where
       inc :: (Ord b, Num b) => [b] -> Maybe ([b], [b])
       inc ps = incPassword ps >>= \ps' -> Just (ps', ps')


-- remember password is stored backwards, and we test for b < a
passwordNotGT :: Ord a => [a] -> [a] -> Bool
passwordNotGT a b =
    let ps = reverse $ zip a b in _checker ps

_checker :: Ord a => [(a,a)] -> Bool
_checker [] = False
_checker ((x,y):_) | y < x = True
_checker (_:xs) = _checker xs


validatedPasswordsBetween :: String -> String -> [[Int]]
validatedPasswordsBetween s1 s2 =
    filter validatePassword $ passwordsBetween s1 s2


main08 :: IO ()
main08 = do
    putStrLn "Day 4: Secure container - part 2"
    putStrLn "https://adventofcode.com/2019/day/4"
    putStrLn "Number of passwords between 137683 and 596253 with extra validation is:"
    print $ length $ map showPassword $ validatedPasswordsBetween "137683" "596253"
