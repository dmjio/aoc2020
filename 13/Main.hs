{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Debug.Trace     (traceShow)
import           Text.Read

import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

main :: IO ()
main = day13

day13 :: IO ()
day13 = do
  putStrLn "day 13"
  [ (read @Int -> earliest)
    , busses
    ] <- lines <$> readFile "day13.txt"
  let busIds
        = catMaybes
        . filter isJust
        . fmap (readMaybe @Int)
        $ (splitOn "," busses :: [String])
  print (findEarliestBus busIds earliest)
  -- print (findSuperEarly busses)
  print (chinese busses)

findEarliestBus :: [Int] -> Int -> Int
findEarliestBus busses start = busId * (distance - start)
  where
    (busId, distance)
      = head
      $ sortBy (compare `on` snd)
      $ zip busses (getClosest start =<< busses)
    getClosest start n
      = take 1
      $ dropWhile (<start)
      $ iterate (+n) 0

busIdsWithOffset :: [String] -> [(Int,Int)]
busIdsWithOffset busses =
  catMaybes (zipWith go busses [0..])
    where
      go "x" _ = Nothing
      go (read -> n) offset = Just (n,offset)

findSuperEarly :: String -> Int
findSuperEarly (busIdsWithOffset .  splitOn "," -> busIds) =
    traceShow busIds (findIt k l busIds)
   where
     !k = fst (head busIds)
     l =
       fromMaybe 1 (fst <$> find (\(_,y) -> y == k) busIds)

allDivisible :: [(Int, Int)] -> Int -> Bool
allDivisible busIds t = do
  foldl' go True busIds
    where
      go acc (x, offset) = acc && (t + offset) `mod` x == 0

-- first solution, correct but slow (566s)
findIt :: Int -> Int -> [(Int,Int)] -> Int
findIt lo hi busIds = go 4300000000 -- start at ~100T
  where
    go !c =
      if allDivisible busIds ((lo * hi * c) - lo)
        then (lo * hi * c) - lo
        else go (c + 1)


chinese :: String -> Int
chinese (busIdsWithOffset .  splitOn "," -> busIds) = fromIntegral x
  where
    Just x = chineseRemainder
      [ (fromIntegral (negate offset `mod` busId), fromIntegral busId)
      | (busId, offset) <- busIds
      ]
