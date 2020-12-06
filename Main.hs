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
main = do
  day1
  day2
  day3
  day4
  day5
  day6

readInput :: String -> IO [Int]
readInput name = fmap read . lines <$> readFile name

day1 :: IO ()
day1 = do
  putStrLn "day 1"
  sequence_ [ day1pt1, day1pt2 ]

day1pt1 :: IO ()
day1pt1 = print =<< processDay1Pt1 <$> readInput "day1.txt"

day1pt2 :: IO ()
day1pt2 = print =<< processDay1Pt2 <$> readInput "day1.txt"

processDay1Pt1 :: [Int] -> Int
processDay1Pt1 input
  = head
  [ x * y
  | x : xs <- tails input
  , y <- xs
  , x + y == 2020
  ]

processDay1Pt2 :: [Int] -> Int
processDay1Pt2 input
  = head
  [ x * y * z
  | x:xs <- tails input
  , y:ys <- tails xs
  , z <- ys
  , x + y + z == 2020
  ]

day2 :: IO ()
day2 = do
  putStrLn "day 2"
  sequence_ [ day2pt1, day2pt2 ]

day2pt1 :: IO ()
day2pt1 = print =<< processDay2Pt1 . lines <$> readFile "day2.txt"

day2pt2 :: IO ()
day2pt2 = print =<< processDay2Pt2 . lines <$> readFile "day2.txt"

processDay2Pt1 :: [String] -> Int
processDay2Pt1 = sum . fmap (parseDay2 validDay2PartOne)

processDay2Pt2 :: [String] -> Int
processDay2Pt2 = sum . fmap (parseDay2 validDay2PartTwo)

parseDay2
  :: (String -> Char -> Int -> Int -> Bool)
  -- ^ Password -> Value -> Lo -> High -> Valid
  -> String
  -> Int
parseDay2 predicate xs =
  case words xs of
    [splitOn "-" -> [read @Int -> lo, read @Int -> hi], [val,':'], password] ->
      popCount (predicate password val lo hi)

validDay2PartOne :: String -> Char -> Int -> Int -> Bool
validDay2PartOne pw c lo hi =
  let
    len = length (filter (==c) pw)
  in
    len >= lo && len <= hi

validDay2PartTwo :: String -> Char -> Int -> Int -> Bool
validDay2PartTwo pw val x1 x2 =
  (pw !! (x1 - 1) == val) `xor` (pw !! (x2 - 1) == val)

day3 :: IO ()
day3 = do
  putStrLn "day 3"
  xs <- lines <$> readFile "day3.txt"
  let rowSize   = length (head xs)
      totalRows = length xs
      mapping   = populateMap xs
  print $ day3pt1 mapping rowSize
  print $ day3pt2 mapping rowSize

day3pt1 :: M.Map (Int,Int) Type -> Int -> Int
day3pt1 = travelAccum (3,1)

day3pt2 :: M.Map (Int,Int) Type -> Int -> Int
day3pt2 m r =
  product
  [ travelAccum (1,1) m r
  , travelAccum (3,1) m r
  , travelAccum (5,1) m r
  , travelAccum (7,1) m r
  , travelAccum (1,2) m r
  ]

travelAccum
  :: (Int,Int)
  -- ^ map
  -> M.Map (Int,Int) Type
  -- ^ x and y accumulators
  -> Int
  -- ^ col count
  -> Int
travelAccum (xAcc, yAcc) m rowSize = go (0,0) 0
  where
    go (!x, !y) !acc = do
      let newKey = (x + xAcc, y + yAcc)
      case M.lookup (x `mod` rowSize, y) m of
        Just Tree ->
          go newKey (acc + 1)
        Just Square ->
          go newKey acc
        Nothing ->
          acc

populateMap :: [String] -> M.Map (Int,Int) Type
populateMap gs = go 0 0 gs mempty
  where
    go x y [] m = m
    go x y (('.':ls):lls) m =
      go (x+1) y (ls:lls) (M.insert (x,y) Square m)
    go x y (('#':ls):lls) m =
      go (x+1) y (ls:lls) (M.insert (x,y) Tree m)
    go x y ([]:lls) m = go 0 (y+1) lls m

data Type = Square | Tree
  deriving (Show, Eq)

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

day5 :: IO ()
day5 = do
  putStrLn "day 5"
  ls <- lines <$> readFile "day5.txt"
  let rowCols = fmap toRowCol ls
      seatIds = sort (fmap toSeatId rowCols)
  print $ day5pt1 rowCols
  print $ day5pt2 seatIds

day5pt1 :: [(Int,Int)] -> Int
day5pt1 rowCols = maximum (fmap toSeatId rowCols)

day5pt2 :: [Int] -> Int
day5pt2 seatIds = findSeatId $ (flip zip =<< tail) seatIds
  where
    findSeatId (x:xs)
      | fst x + 1 == snd x = findSeatId xs
      | otherwise = fst x + 1

toSeatId :: (Int,Int) -> Int
toSeatId (row,col) = (row * 8) + col

toRowCol :: String -> (Int,Int)
toRowCol xs =
  let
    row = take 7 xs
    col = take 3 (drop 7 xs)

    (loRow, hiRow) = foldl' search (0,127) row
    (loCol, hiCol) = foldl' search (0,7) col

    search (lo,hi) 'F' = (lo, (hi + lo) `div` 2)
    search (lo,hi) 'L' = (lo, (hi + lo) `div` 2)
    search (lo,hi) 'B' = (1 + (hi + lo) `div` 2, hi)
    search (lo,hi) 'R' = (1 + (hi + lo) `div` 2, hi)

    exactRow =
      if last row == 'F'
      then loRow
      else hiRow

    exactCol =
      if last col == 'R'
      then hiCol
      else loCol
  in
    (exactRow,exactCol)

day6 :: IO ()
day6 = do
  putStrLn "day 6"
  day6pt1
  day6pt2

day6pt1 :: IO ()
day6pt1 = do
  ls <- lines <$> readFile "day6.txt"
  let groups = splitOn [""] ls
  print $ sum (length . nub . concat <$> groups)
  print $ sum (length . foldl1' intersect <$> groups)

