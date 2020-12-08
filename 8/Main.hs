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
main = day8

day8 :: IO ()
day8 = do
  putStrLn "day 8"
  insts <- fmap parseCode . lines <$> readFile "day8.txt"
  print $ printAccOnInfiniteLoop insts
  print $ flipInstructionOnLoopDetection insts

-- | When encountering an instruction that has already been used
-- Print the accumulator.
printAccOnInfiniteLoop
  :: [Inst]
  -- ^ Program instructions
  -> Int
printAccOnInfiniteLoop insts = go [] 0 0
  where
    go pcs !pc !acc
      | pc `elem` pcs = acc
      | otherwise =
          case insts !! pc of
            Nop _      ->
              go (pc : pcs) (pc + 1) acc
            Jmp offset ->
              go (pc : pcs) (offset + pc) acc
            Acc acc'   ->
              go (pc : pcs) (pc + 1) (acc + acc')

-- | Return the accumulator when the program has terminated
-- When encountering infinite loops, flip instructions, and restart execution
-- Do so until the program cleanly terminates
flipInstructionOnLoopDetection
  :: [Inst]
  -> Int
flipInstructionOnLoopDetection xs = go xs xs [] 0 0 0
  where
    go os insts pcs !pc !acc !flipIdx
      | pc `elem` pcs =
          go os (flipInstrAt flipIdx os) [] 0 0 (flipIdx + 1)
      | pc == length os = acc
      | otherwise =
          case insts !! pc of
            Nop _      ->
              go os insts (pc : pcs) (pc + 1) acc flipIdx
            Jmp offset ->
              go os insts (pc : pcs) (offset + pc) acc flipIdx
            Acc acc'   ->
              go os insts (pc : pcs) (pc + 1) (acc + acc') flipIdx

-- | Flips instruction at specified index
flipInstrAt :: Int -> [Inst] -> [Inst]
flipInstrAt needle = go 0
  where
    go _ [] = []
    go idx (h:hs)
      | idx == needle = flipInst h : hs
      | otherwise = h : go (idx + 1) hs

-- | Flips instruction
flipInst :: Inst -> Inst
flipInst (Nop x) = Jmp x
flipInst (Jmp x) = Nop x
flipInst (Acc x) = Acc x

data Inst
  = Nop Int
  | Jmp Int
  | Acc Int
  deriving (Show)

parseCode :: String -> Inst
parseCode (splitOn " " -> inst) =
  case inst of
    ["nop",'+':x] -> Nop (read x)
    ["nop",'-':x] -> Nop (negate (read x))
    ["acc",'+':x] -> Acc (read x)
    ["acc",'-':x] -> Acc (negate (read x))
    ["jmp",'+':x] -> Jmp (read x)
    ["jmp",'-':x] -> Jmp (negate (read x))
