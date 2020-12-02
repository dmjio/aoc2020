{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bits
import Control.Monad
import Data.List
import Data.List.Split

type Parsed = Int

readInput :: String -> IO [Parsed]
readInput name = fmap read . lines <$> readFile name

day1 :: IO ()
day1 = do
  putStrLn "day 1"
  sequence_ [ day1pt1, day1pt2 ]

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

main :: IO ()
main = day1 >> day2

