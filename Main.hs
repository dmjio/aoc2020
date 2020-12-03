{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE BangPatterns#-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.Bits
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as M
import           Debug.Trace

readInput :: String -> IO [Int]
readInput name = fmap read . lines <$> readFile name

day1 :: IO ()
day1 = do
  putStrLn "day 1"
  sequence_ [ day1pt1, day1pt2 ]

day1pt1 :: IO ()
day1pt1 = print =<< processDay1Pt1 <$> readInput "day1.txt"

day1pt2 :: IO ()
day1pt2 = print =<< processDay1Pt2 <$> readInput "day1.txt"

processDay1Pt1 :: [Int] -> Int
processDay1Pt1 input
  = head
  [ x * y
  | x : xs <- tails input
  , y <- xs
  , x + y == 2020
  ]

processDay1Pt2 :: [Int] -> Int
processDay1Pt2 input
  = head
  [ x * y * z
  | x:xs <- tails input
  , y:ys <- tails xs
  , z <- ys
  , x + y + z == 2020
  ]

day2 :: IO ()
day2 = do
  putStrLn "day2"
  sequence_ [ day2pt1, day2pt2 ]

day2pt1 :: IO ()
day2pt1 = print =<< processDay2Pt1 . lines <$> readFile "day2.txt"

day2pt2 :: IO ()
day2pt2 = print =<< processDay2Pt2 . lines <$> readFile "day2.txt"

processDay2Pt1 :: [String] -> Int
processDay2Pt1 = sum . fmap (parseDay2 validDay2PartOne)

processDay2Pt2 :: [String] -> Int
processDay2Pt2 = sum . fmap (parseDay2 validDay2PartTwo)

parseDay2
  :: (String -> Char -> Int -> Int -> Bool)
  -- ^ Password -> Value -> Lo -> High -> Valid
  -> String
  -> Int
parseDay2 predicate xs =
  case words xs of
    [splitOn "-" -> [read @Int -> lo, read @Int -> hi], [val,':'], password] ->
      popCount (predicate password val lo hi)

validDay2PartOne :: String -> Char -> Int -> Int -> Bool
validDay2PartOne pw c lo hi =
  let
    len = length (filter (==c) pw)
  in
    len >= lo && len <= hi

validDay2PartTwo :: String -> Char -> Int -> Int -> Bool
validDay2PartTwo pw val x1 x2 =
  (pw !! (x1 - 1) == val) `xor` (pw !! (x2 - 1) == val)

day3 :: IO ()
day3 = do
  putStrLn "day 3"
  xs <- lines <$> readFile "day3.txt"
  let rowSize   = length (head xs)
      totalRows = length xs
      mapping   = populateMap xs
  print $ day3pt1 mapping rowSize
  print $ day3pt2 mapping rowSize

day3pt1 :: M.Map (Int,Int) Type -> Int -> Int
day3pt1 = travelAccum (3,1)

day3pt2 :: M.Map (Int,Int) Type -> Int -> Int
day3pt2 m r =
  product
  [ travelAccum (1,1) m r
  , travelAccum (3,1) m r
  , travelAccum (5,1) m r
  , travelAccum (7,1) m r
  , travelAccum (1,2) m r
  ]

travelAccum
  :: (Int,Int)
  -- ^ map
  -> M.Map (Int,Int) Type
  -- ^ x and y accumulators
  -> Int
  -- ^ col count
  -> Int
travelAccum (xAcc, yAcc) m rowSize = go (0,0) 0
  where
    go (!x, !y) !acc = do
      let newKey = (x + xAcc, y + yAcc)
      case M.lookup (x `mod` rowSize, y) m of
        Just Tree ->
          go newKey (acc + 1)
        Just Square ->
          go newKey acc
        Nothing ->
          acc

populateMap :: [String] -> M.Map (Int,Int) Type
populateMap gs = go 0 0 gs mempty
  where
    go x y [] m = m
    go x y (('.':ls):lls) m =
      go (x+1) y (ls:lls) (M.insert (x,y) Square m)
    go x y (('#':ls):lls) m =
      go (x+1) y (ls:lls) (M.insert (x,y) Tree m)
    go x y ([]:lls) m = go 0 (y+1) lls m

data Type = Square | Tree
  deriving (Show, Eq)

main :: IO ()
main = day1 >> day2 >> day3

