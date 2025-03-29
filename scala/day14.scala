package day14

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val example = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

lazy val input = Source.fromFile("day14.input").mkString

val reg = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r

def parse(s: String): List[List[Int]] =
  reg.findAllMatchIn(s).map(_.subgroups.map(_.toInt)).toList

extension (i: Int)
  def +%(m: Int) =
    val t = i % m
    if t < 0 then t + m else t

def move(h: Int, w: Int, x: Int, y: Int, dx: Int, dy: Int, t: Int) =
  ((x + dx * t) +% w, (y + dy * t) +% h)

def quadrant(h: Int, w: Int, x: Int, y: Int): Int =
  val mx = (w - 1) / 2
  val my = (h - 1) / 2

  (x.compare(mx), y.compare(my)) match
    case (1, 1)   => 1
    case (-1, 1)  => 2
    case (-1, -1) => 3
    case (1, -1)  => 4
    case _        => 0

def calcPositions(h: Int, w: Int, params: List[List[Int]], t: Int) =
  for List(x, y, dx, dy) <- params
  yield move(h, w, x, y, dx, dy, t)

def part1(h: Int, w: Int, params: List[List[Int]]) =
  val perQuadrant = calcPositions(h, w, params, 100)
    .map({ case (x, y) => quadrant(h, w, x, y) })
    .groupBy(identity)
    .mapValues(_.length)

  perQuadrant(1) * perQuadrant(2) * perQuadrant(3) * perQuadrant(4)

def runExample = part1(7, 11, parse(example))

def runPart1 = part1(103, 101, parse(input))

extension (l: List[Char])
  def group: List[List[Char]] = l match
    case List() => List()
    case xs =>
      val (as, bs) = xs span { xs.head == _ }
      as :: bs.group

def entropy(h: Int, w: Int, params: List[List[Int]], t: Int) =
  val positions = calcPositions(h, w, params, t).toSet

  val map = List.tabulate(h, w) { (x, y) =>
    if positions.contains((x, y)) then '|' else '.'
  }
  map.flatten.group.length

def printFloor(h: Int, w: Int, params: List[List[Int]], t: Int) =
  val positions = calcPositions(h, w, params, t).toSet

  for x <- 0 until w
  do
    for y <- 0 until h do
      val c = if positions.contains((x, y)) then '|' else '.'
      print(c)
    println()

def part2(h: Int, w: Int, params: List[List[Int]]) =
  (1 to 10000)
    .map(t => entropy(h, w, params, t))
    .zipWithIndex
    .minBy(_._1)
    .pipe(_._2 + 1)

def runPart2 = part2(103, 101, parse(input))
