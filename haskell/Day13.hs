{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day13 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra (chunksOf)
import Data.Maybe (catMaybes, mapMaybe)
import Numeric.LinearAlgebra
import Text.RawString.QQ
import Text.Regex.TDFA

example :: String
example =
  [r|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|]

parse :: String -> [[Double]]
parse str = getAllTextMatches (str =~ "[0-9]+") <&> read & chunksOf 6

isRoundInteger :: Double -> Bool
isRoundInteger x = x == fromIntegral (round x)

roundDouble :: Double -> Double
roundDouble = fromIntegral . round

columns :: [Double] -> [Matrix Double]
columns xs = map col (chunksOf 2 xs)

-- $> [va,vb,prize] = columns (parse example & head)

whenRank2 :: Matrix Double -> Matrix Double -> Maybe Int
whenRank2 mat prize = if scale a (mat ¿ [0]) + scale b (mat ¿ [1]) == prize then Just $ 3 * round a + round b else Nothing
  where
    (Just (toLists -> [[roundDouble -> a], [roundDouble -> b]])) = linearSolve mat prize

-- $> whenRank2 (va ||| vb) prize

toMaybe :: Bool -> a -> Maybe a
toMaybe b x = if b then Just x else Nothing

-- rank 1 case is not relevant to the puzzle input but i have written it anyways for exercise
whenRank1 :: Matrix Double -> Matrix Double -> Maybe Int
whenRank1 mat prize = if null sol then Nothing else Just $ round $ minimum sol
  where
    (toLists -> [[roundDouble -> a]]) = (mat ¿ [0]) <\> prize
    (toLists -> [[roundDouble -> b]]) = (mat ¿ [1]) <\> prize
    pa = scale a (mat ¿ [0])
    pb = scale b (mat ¿ [1])
    sol = catMaybes [toMaybe (pa == prize) (3 * a), toMaybe (pb == prize) b]

-- $> [va,vb,prize] = columns [1,2,2,4,1,2]

-- $> whenRank1 (va ||| vb) prize

calculateCost :: [Matrix Double] -> Maybe Int
calculateCost [va, vb, prize]
  | rank (va ||| vb) == 2 = whenRank2 (va ||| vb) prize
  | otherwise = whenRank1 (va ||| vb) prize

part1 :: [[Double]] -> Int
part1 xss = mapMaybe (calculateCost . columns) xss & sum

-- $> part1 (parse example)

-- $> input <- (readFile "day13.input")

-- $> part1 (parse input)

fixPrice :: [Matrix Double] -> [Matrix Double]
fixPrice [ma, mb, mprize] = [ma, mb, add (col [10000000000000, 10000000000000]) mprize]

part2 :: [[Double]] -> Int
part2 xss = mapMaybe (calculateCost . fixPrice . columns) xss & sum

-- $> part2 (parse input)
