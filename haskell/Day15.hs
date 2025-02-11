module Day15 where

import Control.Category ((>>>))
import Control.Monad (forM_, void, when)
import Control.Monad.State
import Data.Bool (bool)
import Data.Function (on)
import Data.List (find, groupBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Tuple (swap)
import Lens.Micro.Platform
import Text.RawString.QQ
import Text.Regex.TDFA
import Prelude hiding (Left, Right, map)

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

example' =
  [r|#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^|]

regMap = [r|[#.O@]+|]

regMoves = [r|[<^>v]+|]

data Object = Wall | Box | Space | Robot | LBox | RBox deriving (Eq, Show)

data World = World {_map :: Map (Int, Int) Object, _robot :: (Int, Int)}

makeLenses ''World

data Direction = Up | Down | Left | Right deriving (Enum, Ord, Eq, Show)

instance Show World where
  show w = (fmap . fmap) objectToChar strMap & unlines
    where
      strMap = groupBy ((==) `on` (fst . fst)) (Map.toList (w ^. map)) <&> fmap snd

charDirection = [('^', Up), ('v', Down), ('>', Right), ('<', Left)]

charObject = [('#', Wall), ('.', Space), ('@', Robot), ('O', Box), ('[', LBox), (']', RBox)]

objectToChar :: Object -> Char
objectToChar o = let Just (c, _) = find ((== o) . snd) charObject in c

charToObject :: Char -> Object
charToObject c = let Just (_, o) = find ((== c) . fst) charObject in o

charDirectionMap = Map.fromList charDirection

directionCharMap = Map.fromList (charDirection <&> swap)

charToDirection c = charDirectionMap Map.! c

directionToChar d = directionCharMap Map.! d

strToWorld strMap = World (Map.fromList map') robot'
  where
    robot' = filter (snd >>> (== Robot)) map' & fst . head
    map' = do
      (i, r) <- zip [0 ..] strMap
      (j, c) <- zip [0 ..] r
      pure ((i, j), charToObject c)

(x, y) |+| (x', y') = (x + x', y + y')

transform :: Direction -> (Int, Int) -> (Int, Int)
transform dir coords = coords |+| (dirMap Map.! dir)
  where
    dirMap = Map.fromList $ zip (enumFrom Up) [(-1, 0), (1, 0), (0, -1), (0, 1)]

type WorldState a = StateT World Maybe a

try :: WorldState () -> WorldState ()
try ws = do
  s <- get
  case runStateT ws s of
    Nothing -> pure ()
    Just (a, s') -> void (put s')

move :: Direction -> WorldState ()
move dir = do
  s <- get
  let oldPos = s ^. robot
  let newPos = transform dir oldPos
  let o = s ^?! map . ix newPos
  case o of
    Wall -> lift Nothing
    Box -> modify (robot .~ newPos) >> move dir
    Space -> pure ()
  modify (map . ix newPos .~ (s ^?! map . ix oldPos))
  modify (map . ix oldPos .~ Space)
  modify (robot .~ newPos)

move' :: Direction -> WorldState ()
move' dir = do
  s <- get
  let oldPos = s ^. robot
  let newPos = transform dir oldPos
  let newPosLeft = transform Left newPos
  let newPosRight = transform Right newPos
  let o = s ^?! map . ix newPos
  case o of
    Wall -> lift Nothing
    Space -> pure ()
    _ -> do
      modify (robot .~ newPos)
      move' dir
      when (dir `elem` [Up, Down]) do
        modify (robot .~ bool newPosLeft newPosRight (o == LBox))
        move' dir
  modify (map . ix newPos .~ (s ^?! map . ix oldPos))
  modify (map . ix oldPos .~ Space)
  modify (robot .~ newPos)

expand :: Map (Int, Int) Object -> Map (Int, Int) Object
expand m = Map.fromList do
  let dup = \case
        Space -> [Space, Space]
        Box -> [LBox, RBox]
        Robot -> [Robot, Space]
        Wall -> [Wall, Wall]
  ((i, j), o) <- Map.toList m
  zip [(i, j * 2), (i, j * 2 + 1)] (dup o)

expandWorld :: World -> World
expandWorld w = w & map %~ expand & robot %~ (\(i, j) -> (i, j * 2))

part1 world dirs = runStateT f world
  where
    f = do
      forM_ dirs $ \d -> do
        try $ move d
      w <- get
      pure $ sum [100 * i + j | ((i, j), obj) <- Map.toList (w ^. map), obj == Box]

part2 (expandWorld -> world) dirs = runStateT f world
  where
    f = do
      forM_ dirs $ \d -> do
        try $ move' d
      w <- get
      pure $ sum [100 * i + j | ((i, j), obj) <- Map.toList (w ^. map), obj == LBox]

parse :: String -> (World, [Direction])
parse s = (strToWorld warehouse, concat moves <&> charToDirection)
  where
    warehouse = getAllTextMatches (s =~ regMap)
    moves = getAllTextMatches (s =~ regMoves) :: [String]

-- $> import Day15

-- $> import Lens.Micro.Platform

-- $> s = parse example'

-- $> part2 (fst s) [Day15.Left]

-- $> uncurry part2 s

-- $> e <- parse <$> (readFile "haskell/day15.input")

-- $> uncurry part2 e

-- $> uncurry part1 e
