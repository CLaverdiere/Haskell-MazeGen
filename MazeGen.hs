-- Haskell Maze Generator
-- Uses a Union / Find structure to remove walls.

import DisjointSet
import Data.List
import System.Random.Shuffle
import System.Random.Mersenne.Pure64 (newPureMT)

-- Settings
height = 30
width = 30
dims = (width, height)
wallStr = "x"
emptyStr = " "

-- Maze dimensions
type Dims = (Int, Int)

-- An edge is also just a 2D int tuple, as the wall between two cells.
type Edge = (Int, Int)

-- Coordinate pairs readable by Python (PIL) (x1, y1, x2, y2).
type CoordPair = (Int, Int, Int, Int)

-- A maze is just an arbitrary collection of walls.
type Maze = [Edge]

-- Cartesian distance between two coordinates.
dist :: Edge -> Edge -> Int
dist (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

-- Return a list of edges for a maze with given dimensions.
-- Excludes boundary walls.
genEdges :: Dims -> [Edge]
genEdges d@(w, h) = filter (adjacent d) pairs
  where
    el = [0..(w*h)-1]
    pairs = perms2 el

-- Given edges and dimensions, return coordinate pairs for each edge.
genCoord :: Dims -> Edge -> CoordPair
genCoord (w, h) (p1, p2) =
  let p1d = (p1 `div` h)
      p2d = (p2 `div` h)
      p1m = (p1 `mod` w)
  in if p1d == p2d then (p1m+1, p1d, p1m+1, p1d+1)
                   else (p1m, p2d, p1m+1, p2d)

-- Return coordinate pairs of maze boundary.
genBoundary :: Dims -> [CoordPair]
genBoundary (w, h) =
     [(x1,h1,x1+1,h1) | x1 <- [0..w-1], h1 <- [0, h]]
  ++ [(w1,y1,w1,y1+1) | y1 <- [0..h-1], w1 <- [0, w]]

-- Decide if two cells are adjacent.
adjacent :: Dims -> Dims -> Bool
adjacent (w, h) (x, y) = dh + dw == 1
  where
    dh = abs ((y `div` w) - (x `div` w))
    dw = abs ((y `mod` w) - (x `mod` w))

-- Return coordinate pairs for maze given random edge input.
genMaze :: Dims -> [Edge] -> [(DSet Int)] -> [CoordPair]
genMaze (w, h) edges sets = inner_edges ++ outer_edges
  where
    (m_edges, m_sets) = foldl nextUnion (edges, sets) edges
    inner_edges = map (genCoord (w, h)) m_edges
    outer_edges = genBoundary (w, h)

-- Unique 2-tuples
perms2 xs = concat $ zipWith (zip . repeat) xs $ tails xs

main = do
  gen <- newPureMT
  print $ dims
  print $ genMaze dims (shuffle' edges (length edges) gen) sets
    where
      cells = [0..(width*height)]
      sets = makeSet cells
      edges = genEdges dims
