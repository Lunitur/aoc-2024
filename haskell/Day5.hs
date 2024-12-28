module Day5 where

import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Parsec
import Text.RawString.QQ

example :: String
example =
  [r|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|]

inputParser :: Parsec String () (Set (Int, Int), [[Int]])
inputParser = do
  orders <- many1 order
  newline
  updates <- many1 update
  return (Set.fromList orders, updates)

number :: ParsecT String u Identity Int
number = do
  i <- many1 digit
  return (read i :: Int)

update :: ParsecT String u Identity [Int]
update = do
  pages <- sepBy1 number (char ',')
  void endOfLine <|> eof
  return pages

order :: ParsecT String u Identity (Int, Int)
order = do
  i1 <- number
  char '|'
  i2 <- number
  endOfLine
  return (i1, i2)

part1 :: (Set (Int, Int), [[Int]]) -> Int
part1 (os, us) = correctUpdates <&> (\xs -> xs !! (length xs `div` 2)) & sum
  where
    correctUpdates = filter (isCorrect os) us

isCorrect :: Set (Int, Int) -> [Int] -> Bool
isCorrect os u = and [(h, j) `Set.member` os || (j, h) `Set.notMember` os | i@(h : _) <- List.tails u, j <- tail i]

part2 :: (Set (Int, Int), [[Int]]) -> Int
part2 (os, us) = (incorrectUpdates <&> (\xs -> xs !! (length xs `div` 2)) . order') & sum
  where
    order' = List.sortBy cmp
    incorrectUpdates = filter (not . isCorrect os) us
    cmp x y
      | (x, y) `Set.member` os = LT
      | (y, x) `Set.member` os = GT
      | otherwise = EQ
