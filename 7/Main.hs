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
main = day7

parseBag :: String -> (String, [(String,Int)])
parseBag xs = do
  let y:ys =
        fmap (filter (not . isPrefixOf "bag") . words)
          (splitOn "contain" xs)
      getVals ["no","other"] = []
      getVals (chunksOf 3 -> chunks) = fmap parseVal chunks
      parseVal [read -> n,x,y] = (unwords [x,y], n)
  (unwords y, getVals (concat ys))

countHasShinyGoldBag
  :: M.Map String [(String,Int)]
  -> Int
countHasShinyGoldBag m = sum $ fmap go (M.keys m)
  where
    go key =
      case M.lookup key m of
        Nothing -> 0
        Just (fmap fst -> keys)
          | "shiny gold" `elem` keys -> 1
          | otherwise ->
              foldl' max 0 (fmap go keys)

shinyGoldHolds
  :: M.Map String [(String,Int)]
  -> String
  -> Int
shinyGoldHolds m = go
  where
    go key =
      case m M.! key of
        [] -> 0
        kvs -> sum (fmap f kvs)
          where
            f :: (String,Int) -> Int
            f (k,v) = v + (v * go k)

day7 :: IO ()
day7 = do
  putStrLn "day 7"
  ls <- lines <$> readFile "day7.txt"
  let m = M.fromList (fmap parseBag ls)
  print (countHasShinyGoldBag m)
  print (shinyGoldHolds m "shiny gold")
