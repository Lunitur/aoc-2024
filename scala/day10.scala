package day10

import scala.io.Source

val example = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""

def parse(s: String): Array[Array[Int]] =
  s.linesIterator.map(_.map(_.toString.toInt).toArray).toArray

lazy val input = Source.fromFile("day10.input").mkString

def part1(m: Array[Array[Int]]) =
  require(m.length > 0)

  val rows = m.indices
  val cols = m(0).indices

  def rec(x: Int, y: Int): Set[(Int, Int)] =
    if m(x)(y) == 9 then return Set((x, y))

    val sets =
      for
        (i, j) <- List((0, -1), (0, 1), (-1, 0), (1, 0))
        if rows.contains(x + i) &&
          cols.contains(y + j) &&
          m(x)(y) + 1 == m(x + i)(y + j)
      yield rec(x + i, y + j)

    sets.fold(Set.empty)(_ union _)

  val trails = for
    i <- rows
    j <- cols
    if m(i)(j) == 0
  yield rec(i, j)

  trails.flatMap(_.toList).groupBy(identity).mapValues(_.length).values.sum

def part2(m: Array[Array[Int]]) =
  require(m.length > 0)

  val rows = m.indices
  val cols = m(0).indices

  def rec(x: Int, y: Int): Int =
    if m(x)(y) == 9 then return 1

    val sets =
      for
        (i, j) <- List((0, -1), (0, 1), (-1, 0), (1, 0))
        if rows.contains(x + i) &&
          cols.contains(y + j) &&
          m(x)(y) + 1 == m(x + i)(y + j)
      yield rec(x + i, y + j)

    sets.sum

  val trails = for
    i <- rows
    j <- cols
    if m(i)(j) == 0
  yield rec(i, j)

  trails.sum
