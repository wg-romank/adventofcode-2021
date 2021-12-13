import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList.head.split(",").map(_.toInt).toList

    val min = input.min
    val max = input.max

    val totalCost = (mostCommon: Int) => input.map(_ - mostCommon).map(Math.abs).sum

    // todo: this seem to be convex so should'nt scan full range
    val part1 = (min to max).map(
      mc => (mc, totalCost(mc))
    ).minBy(_._2)

    println(s"part1 ${part1}")

    val totalCostpt2 = (mostCommon: Int) => input.map(_ - mostCommon).map(Math.abs).map(v => (1 to v).sum).sum
    val part2 = (min to max).map(
      mc => (mc, totalCostpt2(mc))
    ).minBy(_._2)

    println(s"part2 ${part2}")
  }
}
