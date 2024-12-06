package day6

import scala.annotation.varargs
import scala.io.Source

val example = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".linesIterator.toArray

val input = Source.fromFile("day6.input").getLines().toArray

case class Map(map: Array[String]):
  require(map.nonEmpty)

  val rows = map.indices
  val cols = map(0).indices
  def inbound(x: Int, y: Int): Boolean = rows.contains(x) && cols.contains(y)
  def apply(x: Int, y: Int): Char = map(x)(y)

case class Guard(input: Array[String]):
  val map = Map(input)
  var position =
    (for (i <- map.rows; j <- map.cols; if map(i, j) == '^') yield (i, j)).head

  enum Direction:
    case U, R, D, L
    def next: Direction = Direction.fromOrdinal((this.ordinal + 1) % 4)

  import Direction.*

  var direction = U

  var walked = Set.empty[((Int, Int), Direction)]

  def isInbound: Boolean = map.inbound.tupled(position)

  def isLooped: Boolean = walked.contains((position, direction))

  def move: Unit =
    walked += (position, direction)

    val (x, y) = position
    val p @ (nx, ny) = direction match
      case U => (x - 1, y)
      case R => (x, y + 1)
      case D => (x + 1, y)
      case L => (x, y - 1)

    if map.inbound(nx, ny) && map(nx, ny) == '#'
    then direction = direction.next
    else position = p

def part1(input: Array[String]): Int =
  val g = Guard(input)
  while (g.isInbound) g.move
  g.walked.map(_._1).size

def part2(input: Array[String]): Int =
  var loops = 0
  for
    i <- input.indices
    j <- input(0).indices
    if input(i)(j) == '.'
  do
    val a = input.clone()
    a(i) = a(i).updated(j, '#')
    val g = Guard(a)
    while (g.isInbound && !g.isLooped) g.move
    if g.isLooped then loops += 1
  loops
