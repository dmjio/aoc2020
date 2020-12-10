{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Arrow
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict     as M
import           Data.Maybe

import qualified Data.IntMap.Strict as IM

main :: IO ()
main = day10

day10 :: IO ()
day10 = do
  putStrLn "day 10"
  nums <- sort . fmap (read @Int) . lines <$> readFile "day10.txt"
  let padded = 0 : nums ++ [last nums + 3]
  print (countDifferences padded)
  print (countMemoized (constructGraph padded))

type Graph = IM.IntMap [Int]

-- | Construct Graph where edges link to nodes 1-3 hops away
constructGraph :: [Int] -> Graph
constructGraph xs = do
  let nums = zip xs (tails (drop 1 xs))
  IM.fromList
    [ (x, takeWhile (\y -> y > x && y <= x + 3) xs)
    | (x,xs) <- nums
    ]

-- | Traverse through the graph backwards (starting with terminal node),
-- memoizing in-degree edges. Return index into initial node.
-- Initialize terminal node with an indegree of count 1.
countMemoized :: Graph -> Int
countMemoized graph = do
  let (terminal, _) : xs = reverse (IM.assocs graph)
  foldl' updateMemoTable (IM.singleton terminal 1) xs IM.! 0
    where
      -- | Fold over memo table, accumulating path count
      updateMemoTable memoTable (key, children) = do
        let val = sum $ fmap (\k -> IM.findWithDefault 0 k memoTable) children
        IM.insertWith (+) key val memoTable

-- | For part 1, count path differences of 1,3 between nodes
countDifferences :: (Ord a, Num a) => [a] -> Int
countDifferences xs = do
  let result = sort $ fmap (uncurry subtract) (zip xs (tail xs))
  uncurry (*) $ (length . filter (==1) &&& length . filter (==3)) result
