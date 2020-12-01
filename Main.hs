{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List.Split (splitOn)
import Data.List

type Parsed = Int

main' :: IO ()
main' = process =<< fmap parse . lines <$> readFile "input.txt"

parse :: String -> Parsed
parse = read

process :: [Parsed] -> IO ()
process _ = pure ()

main :: IO ()
main = pure ()
