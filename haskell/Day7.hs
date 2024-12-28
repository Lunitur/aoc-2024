{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7 where

import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.List (foldl')
import Text.RawString.QQ

example :: String
example =
  [r|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|]

parse :: String -> [[Int]]
parse input = lines input <&> (fmap (read . dropColon) . words)
  where
    dropColon = filter (/= ':')

part12 :: [Int -> Int -> Int] -> [[Int]] -> Int
part12 _ [] = 0
part12 ops ((s : is) : iss) = bool 0 s (s `elem` (eval is <$> perms (size - 1))) + part12 ops iss
  where
    size = length is
    perms :: Int -> [[Int -> Int -> Int]]
    perms 0 = [[]]
    perms n = do
      i <- ops
      j <- perms (n - 1)
      return (i : j)
    eval [i] [] = i
    eval (i : is) ops = foldl' (\acc (i, op) -> acc `op` i) i (zip is ops)

concatNumbers :: Int -> Int -> Int
concatNumbers x y = x * 10 ^ floor (logBase 10 (fromIntegral y) + 1) + y

part1ops :: [Int -> Int -> Int]
part1ops = [(*), (+)]

part2ops :: [Int -> Int -> Int]
part2ops = concatNumbers : part1ops

-- $> part12 part1ops  (parse example)

-- $> part12 part2ops (parse example)

-- $> input <- readFile "day7.input"

-- $> part12 part2ops (parse input)
