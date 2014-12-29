-- Quick and dirty Union-Find data structure.

-- TODO don't duplicate head.

module DisjointSet
( dUnion
, dFind
, makeSet
, nextUnion
, DSet
) where

import Data.List

-- TODO remove repeated type
type Edge = (Int, Int)

-- A Disjoint node contains its set's head element, and its own data.
data DNode a = DNode a a deriving (Eq, Show)

-- A Disjoint Set contains several non-overlapping subsets.
type DSet a = [DNode a]

-- Merge two sets, choose the head of the first.
-- TODO choose smaller list for merge.
dUnion :: (DSet a) -> (DSet a) -> (DSet a)
dUnion l1@(x:xs) y = l1 ++ (map (newHead x) y)

-- Find the head of the subset.
dFind :: (DNode a) -> a
dFind (DNode h x) = h

-- Update head on a node.
newHead :: (DNode a) -> (DNode a) -> (DNode a)
newHead (DNode h1 _) (DNode h2 x) = DNode h1 x

-- Create set of singleton subsets.
makeSet :: [a] -> [(DSet a)]
makeSet xs = [[DNode x x] | x <- xs]

-- Find the set that contains a node given a list of sets.
findSetWith :: (Eq a) => a -> [(DSet a)] -> (DSet a)
findSetWith n ds =
  let inSet n' dn' = n' `elem` [x | (DNode h x) <- dn']
  in case find (\ds' -> inSet n ds') ds
     of Just s -> s
        Nothing -> []

-- Test if two nodes have the same head.
sameHead :: (Eq a) => (DNode a) -> (DNode a) -> Bool
sameHead (DNode h1 x1) (DNode h2 x2) = (h1 == h2)

-- Unions two subsets of the disjoint set.
nextUnion :: ([Edge], [DSet Int]) -> Edge -> ([Edge], [DSet Int])
nextUnion (edges, sets) e@(x, y) =
  let (sx, sy) = (findSetWith x sets, findSetWith y sets) in
  case (dFind $ head sx) /= (dFind $ head sy) of
    True -> (delete e edges, (dUnion sx sy) : (delete sx . delete sy) sets)
    False -> (edges, sets)

-- Remove nth element of list.
remove n xs = let (ys, zs) = splitAt n xs in ys ++ (tail zs)
