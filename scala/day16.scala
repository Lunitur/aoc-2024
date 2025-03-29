package day16

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

import cats._
import cats.syntax.all._
import cats.mtl._
import cats.data._

val example = """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"""

val example2 = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"""

def parse(s: String) =
  (for
    (r, i) <- s.linesIterator.zipWithIndex
    (c, j) <- r.zipWithIndex
  yield ((i, j), c)).toMap

import scala.collection.mutable
import scala.io.Source
enum Direction:
  case Left, Up, Right, Down
  def toVec = this match
    case Left  => (0, -1)
    case Right => (0, 1)
    case Up    => (-1, 0)
    case Down  => (1, 0)

import Direction.*

case class Vec(x: Int, y: Int):
  def +(that: Vec) = Vec(this.x + that.x, this.y + that.y)

given tup2Vec: Conversion[(Int, Int), Vec] with
  def apply(x: (Int, Int)): Vec = Vec(x._1, x._2)

given vec2Tup: Conversion[Vec, (Int, Int)] with
  def apply(x: Vec): (Int, Int) = (x.x, x.y)

def input = parse(Source.fromFile("day16.input").mkString)

type TileMap = Map[(Int, Int), Char]
type Node = ((Int, Int), Direction)

def genGraph(tileMap: TileMap): Map[Node, List[(Node, Int)]] =
  (for
    (ix, c) <- tileMap.toList
    dir <- Direction.values
    if '#' != c
  yield
    val rotate = dir match
      case Up | Down    => List((ix, Left) -> 1000, (ix, Right) -> 1000)
      case Left | Right => List((ix, Up) -> 1000, (ix, Down) -> 1000)

    val neighbour: Option[(Node, Int)] = for
      newIx <- Some(dir.toVec + ix)
      c <- tileMap.get(newIx)
      if c != '#'
    yield (newIx: (Int, Int), dir) -> 1
    (ix, dir) -> (rotate ++ neighbour.toList)
  ).toMap

def dijkstra[A](
    graph: Map[A, List[(A, Int)]],
    start: A
): Map[A, Int] = {
  // Collect all unique nodes from both graph keys and edge destinations
  val allNodes = graph.keySet

  // Initialize distances with infinity for all nodes except the start node
  val distances = mutable.Map
    .from(
      allNodes.map(node => node -> Int.MaxValue)
    )
    .withDefaultValue(Int.MaxValue)
  distances(start) = 0

  // Priority queue ordered by ascending distance (min-heap using negative distance for max-heap)
  val queue = mutable.PriorityQueue.empty[(Int, A)](Ordering.by(-_._1))
  queue.enqueue((0, start))

  while (queue.nonEmpty) {
    val (currentDist, currentNode) = queue.dequeue()

    // Only process if we find a better distance than previously recorded
    if (currentDist <= distances(currentNode)) {
      // Explore all neighbors of the current node
      graph(currentNode).foreach { case (neighbor, weight) =>
        val newDist = currentDist + weight
        // Update distance if new path is shorter
        if (newDist < distances(neighbor)) {
          distances(neighbor) = newDist
          queue.enqueue((newDist, neighbor))
        }
      }
    }
  }
  // Convert mutable map to immutable result
  distances.toMap
}

def parentNode(node: Node): Node =
  val x = node._2 match
    case Left  => Vec(0, 1)
    case Right => Vec(0, -1)
    case Up    => Vec(1, 0)
    case Down  => Vec(-1, 0)
  (node._1 + x, node._2)

case class BestPathTilesConfig(
    tileMap: TileMap,
    distances: Map[Node, Int],
    neighbours: Map[Node, List[(Node, Int)]]
)
type BestPathTilesEffect[A] =
  ReaderWriterState[BestPathTilesConfig, String, Set[Node], A]

def bestPathTiles[F[_]: Monad](currentTiles: List[Node])(implicit
    S: Stateful[F, Set[Node]],
    R: Ask[F, BestPathTilesConfig],
    W: Tell[F, String]
): F[Unit] =
  currentTiles.traverse { tile =>
    for
      BestPathTilesConfig(tileMap, distances, neighbours) <- R.ask // R.ask
      _ <- S.modify(_ + tile)
      parentNodes <- neighbours(tile)
        .prepended((parentNode(tile), 1))
        .filter(n => distances(tile) - distances(n._1) == n._2)
        .map(_._1)
        .pure
      _ <- bestPathTiles(parentNodes)
    yield ()
  }.void

def findMinDistance(tm: TileMap) =
  // find letter S
  val sIx = tm.find((_, c) => c == 'S').get._1
  // find letter E
  val eIx = tm.find((_, c) => c == 'E').get._1
  val graph = genGraph(tm)

  val minDistances = dijkstra(graph, (sIx, Right))

  Direction.values.map(d => minDistances((eIx, d))).min

def countBestTiles(tm: TileMap): Int =

  val sIx = tm.find((_, c) => c == 'S').get._1
  val eIx = tm.find((_, c) => c == 'E').get._1
  val neighbours = genGraph(tm).withDefaultValue(List.empty)

  val distances =
    dijkstra(neighbours, (sIx, Right)).withDefaultValue(Int.MaxValue)

  val minDistance = Direction.values.map(d => distances((eIx, d))).min
  val startingTiles = Direction.values.toList
    .map(d => (eIx, d))
    .filter(distances(_) == minDistance)

  bestPathTiles[BestPathTilesEffect](startingTiles)
    .run(BestPathTilesConfig(tm, distances, neighbours), Set.empty)
    .value
    ._2
    .map(_._1)
    .size
