package day8

import scala.io.Source

val example = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

lazy val input = Source.fromFile("day8.input").mkString

def parse(input: String): (Map[Char, List[(Int, Int)]], (Range, Range)) =
  val lines = input.linesIterator.toList
  val ixs = (for
    (r, i) <- lines.zipWithIndex
    (c, j) <- r.zipWithIndex
    if c != '.'
  yield (c, (i, j)))
    .groupMap(_._1)(_._2)
  (ixs, (0 until lines.length, 0 until lines.head.length))

def part1(map: Map[Char, List[(Int, Int)]], range: (Range, Range)) =
  val antinodes = for
    (c, is) <- map.toList
    i @ (x1, y1) <- is
    j @ (x2, y2) <- is
    if i != j
  yield
    val dx = x2 - x1
    val dy = y2 - y1
    (x2 + dx, y2 + dy)

  antinodes
    .filter(ix => range._1.contains(ix._1) && range._2.contains(ix._2))
    .distinct
    .size

def part2(map: Map[Char, List[(Int, Int)]], range: (Range, Range)) =
  val antinodes = for
    (c, is) <- map.toList
    i @ (x1, y1) <- is
    j @ (x2, y2) <- is
    if i != j
  yield
    val dx = x2 - x1
    val dy = y2 - y1
    val p = (x: Int, y: Int) => (y - y1) * dx == (x - x1) * dy
    for
      k <- range._1
      l <- range._2
      if p(k, l)
    yield (k, l)

  antinodes.flatten.distinct.size
