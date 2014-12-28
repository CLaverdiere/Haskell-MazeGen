-- Quick and dirty Union-Find data structure.

-- TODO don't duplicate head.

module DisjointSet
( dUnion
, dFind
, makeSet
, nextUnion
) where

import Data.List

-- A Disjoint Set contains several non-overlapping subsets.
-- Each subset contains a list of elements and a head element.
data DSubset a = DSubset a [a] deriving (Eq, Show)
type DSet a = [DSubset a]

-- Merge the two subsets, choose the head of the first.
dUnion :: (DSubset a) -> (DSubset a) -> (DSubset a)
dUnion (DSubset h xs) (DSubset _ ys)  = DSubset h (xs ++ ys)

-- Find the head of the subset.
dFind :: (DSubset a) -> a
dFind (DSubset h xs) = h

-- Create set of singleton subsets.
makeSet :: [a] -> (DSet a)
makeSet xs = [DSubset x [x] | x <- xs]

-- Unions two subsets of the disjoint set.
nextUnion :: (Eq a) => (DSet a) -> (DSet a)
nextUnion xs =
  case (find (\(x, y) -> dFind x /= dFind y) (perms2 xs)) of
    Just (ys, zs) -> dUnion ys zs : ((delete ys . delete zs) xs)
    Nothing -> xs

-- Remove adjoining wall before union.
-- remAdjUnion :: DSubset -> DSubset -> DSubset
-- remAdjUnion x y =

-- Remove nth element of list.
remove n xs = let (ys, zs) = splitAt n xs in ys ++ (tail zs)

-- Unique 2-tuples
perms2 xs = concat $ zipWith (zip . repeat) xs $ tails xs
