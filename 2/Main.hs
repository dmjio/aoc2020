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
main = day2

day2 :: IO ()
day2 = do
  putStrLn "day 2"
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

