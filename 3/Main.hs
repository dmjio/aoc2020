{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Debug.Trace
import           Text.Read

main :: IO ()
main = day3

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

