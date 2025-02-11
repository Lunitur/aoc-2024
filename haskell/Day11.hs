module Day11 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (group, sort)

example :: String
example = "125 17"

input :: String
input = "0 5601550 3914 852 50706 68 6 645371"

parse :: String -> [Int]
parse = fmap read . words

blinks :: (Ord a, Num a, Read a, Show a) => [a] -> [[(a, Int)]]
blinks rocks = iterate (groupGroupedBlink . concatMap blink) (groupBlink rocks)
  where
    blink (r, m)
      | r == 0 = [(1, m)]
      | even (length (show r)) = let s = show r; (left, right) = splitAt (length s `div` 2) s in [(read left, m), (read right, m)]
      | otherwise = [(r * 2024, m)]

groupBlink :: (Ord a) => [a] -> [(a, Int)]
groupBlink blink = sort blink & group <&> \rs -> (head rs, length rs)

groupGroupedBlink :: (Ord a) => [(a, Int)] -> [(a, Int)]
groupGroupedBlink blink = sort blink & group <&> \rs@((r, m) : _) -> (r, m * length rs)

part :: (Ord a, Num a, Read a, Show a) => Int -> [a] -> Int
part n rocks = blinks rocks & (!! n) <&> snd & sum

main :: IO ()
main = print (part 75 (parse input))
