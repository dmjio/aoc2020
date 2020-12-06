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
main = day5

day5 :: IO ()
day5 = do
  putStrLn "day 5"
  ls <- lines <$> readFile "day5.txt"
  let rowCols = fmap toRowCol ls
      seatIds = sort (fmap toSeatId rowCols)
  print $ day5pt1 rowCols
  print $ day5pt2 seatIds

day5pt1 :: [(Int,Int)] -> Int
day5pt1 rowCols = maximum (fmap toSeatId rowCols)

day5pt2 :: [Int] -> Int
day5pt2 seatIds = findSeatId $ (flip zip =<< tail) seatIds
  where
    findSeatId (x:xs)
      | fst x + 1 == snd x = findSeatId xs
      | otherwise = fst x + 1

toSeatId :: (Int,Int) -> Int
toSeatId (row,col) = (row * 8) + col

toRowCol :: String -> (Int,Int)
toRowCol xs =
  let
    row = take 7 xs
    col = take 3 (drop 7 xs)

    (loRow, hiRow) = foldl' search (0,127) row
    (loCol, hiCol) = foldl' search (0,7) col

    search (lo,hi) 'F' = (lo, (hi + lo) `div` 2)
    search (lo,hi) 'L' = (lo, (hi + lo) `div` 2)
    search (lo,hi) 'B' = (1 + (hi + lo) `div` 2, hi)
    search (lo,hi) 'R' = (1 + (hi + lo) `div` 2, hi)

    exactRow =
      if last row == 'F'
      then loRow
      else hiRow

    exactCol =
      if last col == 'R'
      then hiCol
      else loCol
  in
    (exactRow,exactCol)
