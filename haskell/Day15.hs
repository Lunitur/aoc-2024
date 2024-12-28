{-# LANGUAGE QuasiQuotes #-}

module Day15 where

import Text.RawString.QQ
import Text.Regex.TDFA

example =
  [r|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<|]

regMap = [r|[#.O@]+|]

regMoves = [r|[<^>v]+|]

parse :: String -> ([String], String)
parse s = (warehouse, concat moves)
  where
    warehouse = getAllTextMatches (s =~ regMap)
    moves = getAllTextMatches (s =~ regMoves) :: [String]

-- $> parse example
