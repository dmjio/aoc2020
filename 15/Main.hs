{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Bits
import           Data.Char
import           Data.Function      (on)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.List          (sortBy, find, foldl', isPrefixOf)
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace        (traceShow)
import           Text.Read

main :: IO ()
main = day15

day15 :: IO ()
day15 = do
  putStrLn "day 15"
  let nums = [19,20,14,0,9,1]
  print (findSpoken 2020 nums)
  print (findSpoken 30000000 nums)

findSpoken :: Int -> [Int] -> Int
findSpoken spokenNum nums = do
  let
    spokenTurns = IM.fromList (zipWith (\n t -> (n,[t])) nums [1..])
    spokenCount = IM.fromList (zip nums (repeat 1))
    initial = ( length nums + 1
              , last nums
              , spokenCount
              , spokenTurns
              )
  getSpoken (until (\(getTurn -> t) -> t > spokenNum) go initial)

getTurn :: (a,b,c,d) -> a
getTurn (t,_,_,_) = t

getSpoken :: (a,b,c,d) -> b
getSpoken (_,x,_,_) = x

go :: (Int, Int, IntMap Int, IntMap [Int])
   -> (Int, Int, IntMap Int, IntMap [Int])
go (!turn, !lastSpoken, spokenCount, spokenTurns) = do
   case IM.lookup lastSpoken spokenCount of
    Nothing ->
      ( turn + 1
      , 0
      , IM.insertWith (+) 0 1 spokenCount
      , IM.insertWith (\new old -> new ++ take 1 old) lastSpoken [turn] spokenTurns
      )
    Just 1 ->
      ( turn + 1
      , 0
      , IM.insertWith (+) 0 1 spokenCount
      , IM.insertWith (\new old -> new ++ take 1 old) 0 [turn] spokenTurns
      )
    Just _ ->
      let
        [x,y] = take 2 (spokenTurns IM.! lastSpoken)
        nextSpoken = x-y
      in
        ( turn + 1
        , nextSpoken
        , IM.insertWith (+) nextSpoken 1 spokenCount
        , IM.insertWith (\new old -> new ++ take 1 old) nextSpoken [turn] spokenTurns
        )



