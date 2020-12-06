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
main = do
  day1

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
