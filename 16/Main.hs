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
import           Data.Function      (on)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.List          (sortBy, find, foldl', isPrefixOf, transpose, (\\))
import           Data.List.Split
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe
import           Debug.Trace        (traceShow)
import           Text.Read

main :: IO ()
main = day16

day16 :: IO ()
day16 = do
  putStrLn "day 16"
  ls <- lines <$> readFile "day16.txt"
  let
    (parseFields -> fields)
      : (parseTicket -> ticket)
      : (parseNearby -> nearby)
      : _ = splitOn [""] ls
    valids = findAllValid fields nearby
    transposed = transpose (ticket : valids)
    m = processValidCols (buildIndex ticket fields transposed)
  print (findAllInvalidPartOne fields nearby)
  print (departuresProduct m ticket)

departuresProduct
  :: Map String Int
  -> [Int]
  -- ^ ticket
  -> Int
  -- ^ acc
departuresProduct m ticket =
  product [ i | Just i <- fmap (`M.lookup` m) deps  ]
    where
     deps :: [String]
     deps =
       [ "departure location"
       , "departure station"
       , "departure track"
       , "departure date"
       , "departure time"
       , "departure platform"
       ]

buildIndex :: [Int] -> [Field] -> [[Int]] -> [(Int,[String])]
buildIndex ticket fields = makeValidCols
  where
    makeValidCols cs =
        sortBy (compare `on` length . snd)
          $ zip ticket
          $ flip fmap cs $ \col ->
            [ name f
            | f <- fields
            , all (inRange f) col
            ]

processValidCols
  :: [(Int, [String])]
  -> Map String Int
processValidCols = fst . foldl' go (mempty,[])
  where
    go
      :: (Map String Int, [String])
      -> (Int, [String])
      -> (Map String Int, [String])
    go (m,deleted) (t, names) =
      let
        key = head (names \\ deleted)
      in
       (M.insert key t m, key : deleted)

findAllValid
  :: [Field]
  -- ^ nearby
  -> [[Int]]
  -> [[Int]]
findAllValid fields = filter (all checkInvalid)
  where
    checkInvalid :: Int -> Bool
    checkInvalid x = any (flip inRange x) fields

inRange :: Field -> Int -> Bool
inRange (Field _ (lo1,hi1) (lo2,hi2)) x =
   x >= lo1 && x <= hi1 || x >= lo2 && x <= hi2

outOfRange :: Field -> Int -> Bool
outOfRange f = not . inRange f

findAllInvalidPartOne
  :: [Field]
  -- ^ nearby
  -> [[Int]]
  -> Int
findAllInvalidPartOne fields nearby = sum (go =<< nearby)
  where
    go :: [Int] -> [Int]
    go = filter checkInvalid

    checkInvalid :: Int -> Bool
    checkInvalid x = all (`outOfRange` x) fields

parseTicket :: [String] -> [Int]
parseTicket = fmap read . splitOn "," . head . drop 1

parseNearby :: [String] -> [[Int]]
parseNearby = (fmap . fmap) read . fmap (splitOn ",") . drop 1

parseFields :: [String] -> [Field]
parseFields = fmap parseField

parseField :: String -> Field
parseField xs = do
  case (takeWhile (/=':') xs, dropWhile (/=':') xs) of
    (name, drop 2 -> rest) -> do
      let [low, drop 1 -> hi] = splitOn "or" rest
      Field name (parseRange low) (parseRange hi)

parseRange :: String -> (Int,Int)
parseRange (splitOn "-" -> [read -> l, read -> r]) = (l,r)

data Field
  = Field
  { name :: String
  , lo :: (Int,Int)
  , hi :: (Int,Int)
  } deriving (Show, Eq)
