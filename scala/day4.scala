package day4

import scala.io.Source

val example =
  """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".linesIterator.toList

def input = Source.fromFile("day4.input").getLines.toList

val reg = """(?=XMAS)|(?=SAMX)""".r

def getDiagonals(matrix: List[String]): List[String] =
  val diagonalIndexes = (for i <- matrix.indices; j <- matrix(0).indices
  yield (i, j))
  val topBot = diagonalIndexes.groupBy(k => k._1 - k._2)
  val botTop = diagonalIndexes.groupBy(k => k._2 + k._1)
  def dList(ixs: Map[Int, IndexedSeq[(Int, Int)]]): List[String] =
    (for (_, d) <- ixs
    yield d.map(i => matrix(i._1)(i._2)).mkString).toList

  dList(topBot) ++ dList(botTop)

def part1(input: List[String]) =
  def count(s: String) = reg.findAllMatchIn(s).length

  val d = getDiagonals(input).map(count).sum
  val h = input.map(count).sum
  val v = input.transpose.map(i => count(i.mkString)).sum

  v + h + d

def part2(input: List[String]): Int =
  def x(x: Int, y: Int) =
    var s1 = Array.ofDim[Char](3)
    var s2 = Array.ofDim[Char](3)
    s1(0) = input(x - 1)(y - 1)
    s1(1) = input(x)(y)
    s1(2) = input(x + 1)(y + 1)
    s2(0) = input(x - 1)(y + 1)
    s2(1) = input(x)(y)
    s2(2) = input(x + 1)(y - 1)

    def isMas(s: String) = s match
      case "MAS" | "SAM" => true
      case _             => false

    isMas(s1.mkString) && isMas(s2.mkString)

  (for
    i <- input.indices
    j <- input(0).indices
    if input.indices.containsSlice(i - 1 to i + 1) &&
      input(1).indices.containsSlice(j - 1 to j + 1)
  yield x(i, j)).filter(identity).length
