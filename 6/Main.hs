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
main = day6

day6 :: IO ()
day6 = do
  putStrLn "day 6"
  ls <- lines <$> readFile "day6.txt"
  let groups = splitOn [""] ls
  print $ sum (length . nub . concat <$> groups)
  print $ sum (length . foldl1' intersect <$> groups)
