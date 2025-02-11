module Day1 where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List qualified as List
import Data.Map qualified as Map
import Text.RawString.QQ

example :: ([Int], [Int])
example =
  parse
    [r|3   4
4   3
2   5
1   3
3   9
3   3|]

parse :: String -> ([Int], [Int])
parse input = lines input <&> ((\[i1, i2] -> (i1, i2)) . fmap read . words) & unzip

minDistance :: ([Int], [Int]) -> Int
minDistance lists = bimap sort sort lists & uncurry (zipWith (-)) <&> abs & sum

similarityScore :: ([Int], [Int]) -> Int
similarityScore (l1, l2) = sum similaryList
  where
    countMap = sort l2 & List.group <&> (\x -> (head x, length x)) & Map.fromList
    similaryList = [x * Map.findWithDefault 0 x countMap | x <- l1]
