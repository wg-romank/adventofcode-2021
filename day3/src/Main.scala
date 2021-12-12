import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().filter(_.nonEmpty).toList

    val numBits = input.head.length()

    val initial = List.fill(numBits)((0, 0)).zipWithIndex

    val counters = input.foldLeft(initial) {
      case (items, next) =>
        val lookup = next.chars().toArray()
        items.map {
          case ((ones, zeros), idx) =>
            (if (lookup(idx) == '1') (ones + 1, zeros) else (ones, zeros + 1), idx)
        }
    }.map { case ((ones, zeros), idx) => (ones > zeros, idx) }

    val (mc, lc) = counters.foldLeft[(Int, Int)]((0, 0)) {
      case ((mc, lc), (present, idx)) =>
        val bitValue = Math.pow(2, numBits - idx - 1).toInt
        if (present)
          (mc + bitValue, lc)
        else
          (mc, lc + bitValue)
    }

    val part1 = mc * lc
    println(s"part1 ${part1}")
  }
}
