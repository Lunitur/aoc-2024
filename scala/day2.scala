package day2

import scala.io.Source

val example = parse("""7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""")

lazy val input = parse(Source.fromFile("day2.input").mkString)

def parse(s: String): List[List[Int]] =
  s.linesIterator.map(_.split(" ").map(_.toInt).toList).toList

def isSafeReport(report: List[Int]): Boolean =
  val diffs = report.sliding(2).map { case List(x1, x2) => x1 - x2 }.toList
  diffs.forall(_.abs <= 3) && (diffs.forall(_ < 0) || diffs.forall(_ > 0))

def part1(reports: List[List[Int]]) = reports.filter(isSafeReport).length

def isAlmostSafeReport(report: List[Int]): Boolean =
  (for i <- 0 until report.length yield
    val (l1, l2) = report.splitAt(i)
    l1 ++ l2.tail
  )
    .exists(isSafeReport) || isSafeReport(report)

def part2(reports: List[List[Int]]) = reports.filter(isAlmostSafeReport).length
