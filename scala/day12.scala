package day12

import scala.io.Source
import scala.collection.immutable.ArraySeq

val example = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

lazy val input = Source.fromFile("day12.input").mkString

def parse(s: String): Array[String] = s.linesIterator.toArray

type RegionMap = Array[String]

implicit def bool2int(b: Boolean): Int = if b then 1 else 0

def part1(m: RegionMap): Int =
  val cols = m.indices
  val rows = m(0).indices

  val regions = Array.tabulate(rows.end, cols.end) { (_, _) => 0 }
  var regCounter = 1;

  def fill(x: Int, y: Int): Unit =
    if regions(x)(y) != 0 then return
    regions(x)(y) = regCounter
    for
      (i, j) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
      if rows.contains(x + i) && cols.contains(y + j)
      if m(x + i)(y + j) == m(x)(y)
    do fill(x + i, y + j)

  for
    i <- rows
    j <- cols
    if regions(i)(j) == 0
  do
    fill(i, j)
    regCounter += 1

  val areas = regions.flatten.groupBy(identity).mapValues(_.length)

  def perimeter(x: Int, y: Int) =
    (for
      (i, j) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
      if !rows.contains(x + i) || !cols.contains(y + j) || m(x + i)(y + j) != m(
        x
      )(y)
    yield 1).sum

  val perimeters = Vector.tabulate(rows.end, cols.end) { perimeter }
  (for
    i <- rows
    j <- cols
  yield perimeters(i)(j) * areas(regions(i)(j))).sum

def part2(m: RegionMap) =
  val cols = m.indices
  val rows = m(0).indices

  val regions = Array.tabulate(rows.end, cols.end) { (_, _) => 0 }
  var regCounter = 1;

  def fill(x: Int, y: Int): Unit =
    if regions(x)(y) != 0 then return
    regions(x)(y) = regCounter
    for
      (i, j) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
      if rows.contains(x + i) && cols.contains(y + j)
      if m(x + i)(y + j) == m(x)(y)
    do fill(x + i, y + j)

  for
    i <- rows
    j <- cols
    if regions(i)(j) == 0
  do
    fill(i, j)
    regCounter += 1

  val areas = regions.flatten.groupBy(identity).mapValues(_.length)

  def sides(x: Int, y: Int): List[Boolean] =
    def f(i: Int, j: Int) =
      if rows.contains(x + i) && cols.contains(y + j) then
        regions(x + i)(y + j) != regions(x)(y)
      else true
    List(f(-1, 0), f(1, 0), f(0, -1), f(0, 1))

  val corners = ArraySeq.tabulate(rows.end, cols.end) { (x, y) =>
    val List(u, d, l, r) = sides(x, y)
    (u && r) + (r && d) + (d && l) + (l && u)
  }

  val concaveCorners = ArraySeq.tabulate(rows.end, cols.end) { (x, y) =>
    val List(u, d, l, r) = sides(x, y)
    var c = 0
    if !u && !r then
      val List(_u, _d, _l, _r) = sides(x - 1, y + 1)
      c += _d && _l
    if !u && !l then
      val List(_u, _d, _l, _r) = sides(x - 1, y - 1)
      c += _d && _r
    if !d && !r then
      val List(_u, _d, _l, _r) = sides(x + 1, y + 1)
      c += _u && _l
    if !d && !l then
      val List(_u, _d, _l, _r) = sides(x + 1, y - 1)
      c += _u && _r
    c
  }

  val totalCorners: Map[Int, Int] = (for
    i <- rows
    j <- cols
  yield (regions(i)(j), corners(i)(j) + concaveCorners(i)(j)))
    .groupBy(_._1)
    .mapValues(_.map(_._2).sum)
    .toMap

  (for i <- 1 `until` regCounter
  yield totalCorners(i) * areas(i)).sum
