import scala.io.Source

object Main {
  def part1(measurements: List[Int]): Int =
    measurements
      .foldLeft[(Int, Option[Int])]((0, None)) { case ((larger, last), v) =>
        (if (v > last.getOrElse(Int.MinValue)) larger + 1 else larger, Some(v))
      }
      ._1 - 1

  def part2(measurements: List[Int]): Int =
    measurements.sliding(3).foldLeft[(Int, Int)]((0, 0)) {
      case ((larger, last), item) =>
        val newSum = item.sum
        (if (newSum > last) larger + 1 else larger, newSum)
    }._1 - 1

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().filter(_.nonEmpty).map(_.toInt).toList

    println(part1(input))

    println(part2(input))
  }
}
