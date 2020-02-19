module Utils where

import Data.List (unfoldr)

-- split a list of a list of pairs. best explained by example.
-- Givem [1,2,3,4], return [([], [2,3,4]), ([1], [3,4]), ([1,2],[4]),
-- ([1,2,3],[])]  i.e. 4 pairs with successive elements missing
splits :: [a] -> [([a],[a])]
splits [] = []
splits (a:as) = unfoldr go ([], Just a, as)
  where
      go :: ([a],Maybe a,[a]) -> Maybe (([a],[a]), ([a],Maybe a,[a]))
      go (_, Nothing, _) = Nothing
      go (ls, Just c, []) = Just ((ls,[]), ([], Nothing, []))
      go (ls, Just c, bs@(r:rs)) = Just ((ls,bs), (ls++[c], Just r, rs))


insertIntoList :: (a -> a -> Ordering) -> (a -> a -> Bool) -> a -> [a] -> [a]
insertIntoList fcompare fequal a = insertIntoList' fcompare fequal a []


insertIntoList' :: (a -> a -> Ordering) -> (a -> a -> Bool) -> a -> [a] -> [a] -> [a]
insertIntoList' _ _ a as [] = reverse (a:as)
insertIntoList' fcompare fequal a as (b:bs) =
    case fcompare a b of
        LT -> reverse as ++ (a: stripSame fequal a (b:bs))    -- insert ht item and end
        GT -> insertIntoList' fcompare fequal a (b:as) bs
        EQ -> if fequal a b
                then reverse as ++ (b:bs) -- discard the item and end
                else insertIntoList' fcompare fequal a (b:as) bs


-- remove item a from list [a] if they are equal.
stripSame :: (a -> a -> Bool) -> a -> [a] -> [a]
stripSame f a = filter (not . f a)
