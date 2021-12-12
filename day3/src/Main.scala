import scala.io.Source

object Main {

  def cc(ozc: ((Int, Int), Char)): (Int, Int) = {
    val ((ones, zeros), char) = ozc
    if (char == '1') (ones + 1, zeros) else (ones, zeros + 1)
  }

  def cc2(oz: (Int, Int), char: Char): (Int, Int) = {
    val (ones, zeros) = oz
    if (char == '1') (ones + 1, zeros) else (ones, zeros + 1)
  }

  def computeCounters(input: List[String]): List[(Int, Int)] = {
    val numBits = input.head.length()
    val initial = List.fill(numBits)((0, 0))

    input.foldLeft(initial) { case (items, next) => items.zip(next).map(cc).toList }
  }

  def binStr2Int(input: String): Int = {
    val numBits = input.length()
    input.zipWithIndex.foldLeft(0) {
      case (result, (next, idx)) =>
        if (next == '1') result + Math.pow(2, numBits - idx - 1).toInt else result
    }
  }

  def bitCriteria(input: List[String], criteria: (Int, Int) => Char, bitIdx: Int = 0): Either[String, String] =
    input match {
      case head :: Nil if head.forall(_.isDigit) => Right(head)
      case head :: next if bitIdx >= head.length => Left(s"past end bit ${bitIdx}")
      case Nil => Left(s"something wrong, no values left, bit idx ${bitIdx}")
      case otherwise =>
        val (ones, zeros) = otherwise.map { _(bitIdx) }.foldLeft[(Int, Int)]((0, 0))(cc2)
        val bitValue = criteria(ones, zeros)
        bitCriteria(
          input.filter(_(bitIdx) == bitValue), criteria, bitIdx + 1
        )
    }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().filter(_.nonEmpty).toList

    val counters = computeCounters(input)

    val (mc, lc) = counters.foldLeft[(String, String)](("", "")) {
      case ((mc, lc), (ones, zeros)) =>
        if (ones > zeros)
          (mc :+ '1', lc :+ '0')
        else
          (mc :+ '0', lc :+ '1')
    }

    val part1 = binStr2Int(mc) * binStr2Int(lc)
    println(s"part1 ${part1}")

    val or = binStr2Int(
      bitCriteria(
        input, (ones, zeros) => if (ones >= zeros) '1' else '0'
      ).right.get
    )

    val co2r = binStr2Int(
      bitCriteria(
        input, (ones, zeros) => if (ones < zeros) '1' else '0'
      ).right.get
    )

    val part2 = or * co2r
    println(s"part2 ${part2}")
  }
}
