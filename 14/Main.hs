{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bits
import Data.Char
import Data.Function   (on)
import Data.List       (sortBy, find, foldl', isPrefixOf)
import Data.List.Split
import Data.Maybe
import Debug.Trace     (traceShow)
import Text.Read
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = day14

day14 :: IO ()
day14 = do
  putStrLn "day 14"
  ls <- lines <$> readFile "day14.txt"
  let progs = parseProgs (chunkMask ls)
  print (runProg1 progs)
  print (runProg2 progs)

chunkMask :: [String] -> [[String]]
chunkMask [] = []
chunkMask (x:xs)
  | "mask" `isPrefixOf` x = do
      let more = takeWhile (not . isPrefixOf "mask") xs
          rest = dropWhile (not . isPrefixOf "mask") xs
      (x:more) : chunkMask rest
  | otherwise = []

parseProgs :: [[String]] -> [Prog]
parseProgs = fmap go
  where
    go :: [String] -> Prog
    go ((drop 7 -> mask) : mems) =
      Prog mask (fmap parseMem mems)

    parseMem :: String -> Mem
    parseMem xs = do
      let addr' = takeWhile isDigit . drop 4 $ xs
      Mem (read addr')
        $ read
        . reverse
        . takeWhile isDigit
        . reverse
        $ xs

data Prog
  = Prog
  { mask :: String
  , mems :: [Mem]
  } deriving (Show, Eq)

data Mem = Mem Int Int
  deriving (Show, Eq)

maskedValue :: String -> Int -> Int
maskedValue mask num = do
  foldl' go num (zip [0..] (reverse mask))
    where
      go num (idx, 'X') = num
      go num (idx, '1') = num `setBit` idx
      go num (idx, '0') = num `clearBit` idx

runProg1 :: [Prog] -> Int
runProg1 progs = sum (IM.elems (foldl' go IM.empty progs))
  where
    go imap (Prog mask mems) = do
      foldl' compute imap mems
        where
          compute imap' (Mem k v) =
            IM.insert k (maskedValue mask v) imap'

runProg2 :: [Prog] -> Int
runProg2 progs = sum (IM.elems (foldl' go IM.empty progs))
  where
    go imap (Prog mask mems) = foldl' compute imap mems
      where
        compute imap' (Mem k v) =
          foldr (flip IM.insert v) imap'
            (getAddrs mask k)

getAddrs :: String -> Int -> [Int]
getAddrs mask addr =
  uncurry processFloats
    (foldl go (addr, mempty)
       (zip [0..] (reverse mask)))
    where
      go (addr',floats) (idx, 'X') = (addr', idx:floats)
      go (addr',floats) (idx, '1') = (addr' `setBit` idx, floats)
      go (addr',floats) (idx, '0') = (addr', floats)

processFloats
  :: Int
  -> [Int]
  -> [Int]
processFloats addr floatIdxs =
    applyTransform addr <$> transforms
  where
    transforms :: [[Int -> Int]]
    transforms = fmap make combos

    combos :: [[Int]]
    combos = combinations (length floatIdxs)

    applyTransform :: Int -> [Int -> Int] -> Int
    applyTransform = foldl (flip ($))

    make combo =
      fmap (\(idx, n) ->
              if n == 0
              then (`clearBit` idx)
              else (`setBit` idx))
      (zip floatIdxs combo)

combinations
  :: Int
  -> [[Int]]
combinations x =
  fmap toBits [0 .. (2^x) - 1 ]
    where
      toBits :: Int -> [Int]
      toBits n
        = take x
        $ zipWith (\idx b -> popCount (b `testBit` idx))
            [0..] (repeat n)
