{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Control.Monad.Extra (whenJust)
import Data.FingerTree (FingerTree, ViewR ((:>)))
import Data.FingerTree qualified as FingerTree
import Data.Foldable (Foldable (..), foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl1', group, intersperse, zip4)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), ViewL (..), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Tuple.Extra
import Debug.Trace (traceShow, traceShowId)

example = "2333133121414131402"

data Space = File | Free

parse :: String -> Seq (Maybe Int)
parse (fmap (read . return) -> s) = concatMap expand (zip3 s (intersperse 0 [0 ..]) (cycle [File, Free])) & Seq.fromList
  where
    expand (size, id, File) = replicate size (Just id)
    expand (size, id, Free) = replicate size Nothing

parse' :: String -> (Map Int Int, Map Int Int)
parse' (fmap (read . return) -> s) = let (mSize, mPos, _) = foldl' f (Map.empty, Map.empty, 0) (zip s (intersperse Nothing (fmap Just [0 ..]))) in (mSize, mPos)
  where
    f (mSize, mPos, acc) (size, Nothing) = (mSize, mPos, acc + size)
    f (mSize, mPos, acc) (size, Just id) = (Map.insert id size mSize, Map.insert id acc mPos, acc + size)

part1 :: Seq (Maybe Int) -> Int
part1 seq = f seq & zipWith (*) [0 ..] & sum
  where
    f :: Seq (Maybe Int) -> [Int]
    f ((Nothing :<| seq) :|> Just id) = id : f seq
    f (Just id :<| seq) = id : f seq
    f (seq :|> Nothing) = f seq
    f Seq.Empty = []

part2 :: (Map Int Int, Map Int Int) -> Int
part2 (mSize, mPos) = f (Map.keys mSize & maximum) seq <&> calc & sum
  where
    calc (id, pos) = id * (n (pos + size id - 1) - n (pos - 1))
    n k = k * (k + 1) `div` 2
    seq = Map.toAscList mPos & Seq.fromList
    size = (mSize Map.!)
    findId id = fromJust . Seq.findIndexR ((== id) . fst)

    f :: Int -> Seq (Int, Int) -> Seq (Int, Int)
    f 0 seq = seq
    f id seq
      | freeIx == idIx = f (id - 1) seq
      | otherwise =
          f
            (id - 1)
            ( seq
                & Seq.deleteAt idIx
                & Seq.insertAt (freeIx + 1) (let (prevId, prevPos) = Seq.index seq freeIx in (id, prevPos + size prevId))
            )
      where
        idIx = findId id seq
        freeIx =
          fromMaybe
            idIx
            $ List.findIndex
              (uncurry \(id1, pos1) (id2, pos2) -> id1 == id || pos2 - (pos1 + size id1) >= size id)
              (zip (toList seq) (tail $ toList seq))

main = do
  input <- readFile "day9.input"
  print $ part2 $ parse' input

-- $> parse example

-- $> parse' example

-- $> part1 (parse example)

-- $> part2 $ parse example

-- $> part2 $ parse' example

-- $> input <- readFile "day9.input"

-- $> part2 $ parse'  input
