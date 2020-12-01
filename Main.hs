{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad   (sequence)
import Data.List       (nub, tails)
import Data.List.Split (splitOn)

type Parsed = Int

readInput :: String -> IO [Parsed]
readInput name = fmap read . lines <$> readFile name

day1 :: IO ()
day1 = sequence_ [ day1pt1, day1pt2 ]

day1pt1 :: IO ()
day1pt1 = print =<< processDay1Pt1 <$> readInput "day1.txt"

day1pt2 :: IO ()
day1pt2 = print =<< processDay1Pt2 <$> readInput "day1.txt"

processDay1Pt1 :: [Parsed] -> Parsed
processDay1Pt1 input
  = head
  [ x * y
  | x : xs <- tails input
  , y <- xs
  , x + y == 2020
  ]

processDay1Pt2 :: [Parsed] -> Parsed
processDay1Pt2 input
  = head
  [ x * y * z
  | x:xs <- tails input
  , y:ys <- tails xs
  , z <- ys
  , x + y + z == 2020
  ]

main :: IO ()
main = day1
