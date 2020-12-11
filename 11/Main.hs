{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad   (forM_)
import           Data.List       (delete)
import qualified Data.Map.Strict as M
import           Data.Maybe      (isJust, catMaybes, listToMaybe)

main :: IO ()
main = day11

day11 :: IO ()
day11 = do
  putStrLn "day 11"
  seats <- (fmap . fmap) parseSeat . lines <$> readFile "day11.txt"
  let
    col = length (head seats) - 1
    row = length seats - 1
    grid = createGrid row col seats
    occupiedPt1 = 4
    occupiedPt2 = 5
  print (findOccupied getAdjacentPt1 occupiedPt1 row col grid)
  print (findOccupied getAdjacentPt2 occupiedPt2 row col grid)

findOccupied
  :: GetAdjacency
  -> Int
  -> Int
  -> Int
  -> Grid
  -> Int
findOccupied adjacencyFun occupied r c = go 0
  where
    go !num currentGrid = do
      let newGrid = flipSeats adjacencyFun occupied r c currentGrid
      if newGrid == currentGrid
        then M.size (M.filter (==Occupied) newGrid)
        else go (num + 1) newGrid

printGrid :: Int -> Int -> Grid -> IO ()
printGrid rowSize colSize grid = do
  forM_ [ 0 .. rowSize ] $ \row -> do
    putChar '\n'
    forM_ [ 0 .. colSize ] $ \col -> do
      putChar (printSeat (grid M.! (row, col)))
  putStrLn mempty

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat '#' = Occupied
parseSeat 'L' = Empty
parseSeat _   = error "unknown seat"

printSeat :: Seat -> Char
printSeat Floor    = '.'
printSeat Occupied = '#'
printSeat Empty    = 'L'

type Grid = M.Map (Int,Int) Seat

createGrid :: Int -> Int -> [[Seat]] -> Grid
createGrid row col seats = do
  M.fromList (zip rowCols (concat seats))
    where
      rowCols = (,) <$> [0..row] <*> [0..col]

adjacentCoords :: [(Int, Int)]
adjacentCoords = delete (0,0) coords
  where
    coords = (,) <$> [1,0..(-1)] <*> [1,0..(-1)]

getAdjacentPt1 :: (Int,Int) -> Grid -> [Seat]
getAdjacentPt1 (x,y) grid = catMaybes $ do
  fmap (`M.lookup` grid)
    [ (x + x', y + y')
    | (x', y') <- adjacentCoords
    ]

getAdjacentPt2 :: (Int,Int) -> Grid -> [Seat]
getAdjacentPt2 (x,y) grid = catMaybes $ do
  findNextSeat (x,y) grid <$> adjacentCoords

findNextSeat :: (Int, Int) -> Grid -> (Int, Int) -> Maybe Seat
findNextSeat (x,y) grid (deltaX, deltaY) =
   listToMaybe
    $ dropWhile (==Floor)
    $ catMaybes
    $ takeWhile isJust
    $ fmap (`M.lookup` grid)
    $ zip rows cols
  where
    rows = iterate (+deltaX) (x+deltaX)
    cols = iterate (+deltaY) (y+deltaY)

flipSeat
  :: GetAdjacency
  -> Int
  -> Grid
  -> (Int,Int)
  -> ((Int,Int), Seat)
flipSeat getAdjacentFun countOccupied grid key = do
  let adjacent = getAdjacentFun key grid
  case grid M.! key of
    Empty
      | null (filter (==Occupied) adjacent) -> (key, Occupied)
      | otherwise -> (key, Empty)
    Occupied
      | length (filter (==Occupied) adjacent) >= countOccupied -> (key, Empty)
      | otherwise -> (key, Occupied)
    Floor -> (key, Floor)

type GetAdjacency = (Int,Int) -> Grid -> [Seat]

flipSeats :: GetAdjacency -> Int -> Int -> Int -> Grid -> Grid
flipSeats getAdjFun occupied rowSize colSize grid =
  M.fromList (flipSeat getAdjFun occupied grid <$> coords)
    where
      coords = (,) <$> [0..rowSize] <*> [0..colSize]

data Seat
  = Occupied
  | Empty
  | Floor
  deriving (Show, Eq, Ord)
