-- Haskell Maze Generator

import DisjointSet

import System.Random
import Data.List

-- TODO use Union/Find algorithm

-- Settings
height = 2
width = 2
wallStr = "x"
emptyStr = " "

-- Maze dimensions
type Dims = (Int, Int)

-- A maze coordinate is just a 2D int tuple.
type MazeCoord = (Int, Int)

-- A wall is defined by a start coordinate, and end coordinate.
type Wall = (MazeCoord, MazeCoord)

-- A maze is just an arbitrary collection of walls.
type Maze = [Wall]

dist :: MazeCoord -> MazeCoord -> Int
dist (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

-- Print simple text representation of maze.
mazeStr :: Maze -> Dims -> String
mazeStr xs (dx, dy) = concatMap sym coords
  where
    coords = [(x, y) | x <- [0..dx], y <- [0..dy]]
    combs = concatMap (\((a,b),(c,d)) -> [(a,b),(c,d)]) xs
    sym (x, y)
      | (x, y) `elem` combs = wallStr ++ formatStr
      | otherwise           = emptyStr ++ formatStr
        where
          formatStr = if x == dx then "\n" else ""

-- Return a list of available walls at a given coordinate.
coordPairs :: (Int, Int) -> MazeCoord -> [Wall]
coordPairs (dx, dy) (x, y)
  | (x < 0) || (x > dx) || (y < 0) || (y > dy) = []
  | otherwise = [c | (x', f, c) <- (zip3 xs fs cs), (f x') == True]
    where
      xs = [x, x, y, y]
      fs = [(>0), (<dx), (>0), (<dy)]
      cs = [((x-1, y), (x, y)), ((x, y), (x+1, y)),
            ((x, y-1), (x, y)), ((x, y), (x, y+1))]

main = do
  -- putStrLn $ mazeStr maze (width, height)
  -- TODO use random union ordering instead.
  print $ jset
    where
      grid = [(x, y) | x <- [0..width], y <- [0..height]]
      maze = nub $ concatMap (coordPairs (width, height)) grid
      dset = makeSet maze
      jset = foldr (\_ y -> nextUnion y) dset [1..length dset]
