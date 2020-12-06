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
main = day4

day4 :: IO ()
day4 = do
  putStrLn "day 4"
  ls <- lines <$> readFile "day4.txt"
  let newLines = fmap (intercalate " ") (splitOn [""] ls)
  print $ day4pt1 newLines
  print $ day4pt2 newLines

day4pt1 :: [String] -> Int
day4pt1 = solve4 (const mempty)

day4pt2 :: [String] -> Int
day4pt2 = solve4 $ \m ->
  [ validByr (m M.! "byr")
    && validIyr (m M.! "iyr")
    && validEyr (m M.! "eyr")
    && validHgt (m M.! "hgt")
    && validHcl (m M.! "hcl")
    && validEcl (m M.! "ecl")
    && validPid (m M.! "pid")
  ]

solve4 :: (M.Map String String -> [Bool]) -> [String] -> Int
solve4 validationRules = foldl' countValidPassports 0
  where
    countValidPassports :: Int -> String -> Int
    countValidPassports acc line = do
       let fillMap m word | [k,v] <- splitOn ":" word = M.insert k v m
           m = foldl' fillMap mempty (words line)
           allFound = isJust (traverse (`M.lookup` m) day4Keys)
       if allFound && and (validationRules m)
         then
           acc + 1
         else
           acc

day4Keys =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

ranged :: (Int,Int) -> String -> Bool
ranged (x',y') str =
  case readMaybe str of
    Nothing -> False
    Just (x :: Int) ->
      x >= x' && x <= y'

validHgt :: String -> Bool
validHgt xs =
  case readMaybe @Int (takeWhile isDigit xs) of
    Nothing -> False
    Just num ->
      case reverse (take 2 (reverse xs)) of
        "cm" -> ranged (150,193) (takeWhile isDigit xs)
        "in" -> ranged (59,76) (takeWhile isDigit xs)
        _ -> False

validHcl :: String -> Bool
validHcl ('#':str)
  | and [ length str == 6
        , all (\x -> x `elem` ['0'..'9'] || x `elem` ['a'..'f']) str
        ] = True
validHcl _ = False

validEcl :: String -> Bool
validEcl xs = xs `elem` ["amb", "blu","brn","gry","grn","hzl","oth"]

validPid :: String -> Bool
validPid xs = length (takeWhile isDigit xs) == 9

validByr, validIyr, validEyr :: String -> Bool
validByr = ranged (1920,2002)
validIyr = ranged (2010,2020)
validEyr = ranged (2020,2030)

