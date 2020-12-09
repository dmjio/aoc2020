{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Debug.Trace
import           Text.Read

main :: IO ()
main = day9

day9 :: IO ()
day9 = do
  putStrLn "day 9"
  nums <- fmap read . lines <$> readFile "day9.txt"
  let partOne = findFirstNonSum 25 nums
  print partOne
  print $ addSmallestAndLargest (contiguousSet nums partOne)

-- | Find the first element that is not a sum of any combination of
-- two elements in the immediate previous n numbers
-- (known as the preamble). The preamble count is 25 in this case.
findFirstNonSum :: Int -> [Int] -> Int
findFirstNonSum count xs = do
  let (preamble, num : _) = splitAt count xs
      preambleSums = uncurry (+) <$> liftA2 (,) preamble preamble
  if num `elem` preambleSums
    then findFirstNonSum count (tail xs)
    else num

-- | Find a contiguous set of n numbers (greater than 2) that sum to
-- the target number (target number being the result of part 1).
contiguousSet :: [Int] -> Int -> [Int]
contiguousSet xs target = do
  let found  = length $ takeWhile (<= target) (scanl1 (+) xs)
      window = take found xs
  if sum window == target && length window >= 2
    then window
    else contiguousSet (drop 1 xs) target

addSmallestAndLargest :: [Int] -> Int
addSmallestAndLargest = uncurry (+) . (head &&& last) . sort
