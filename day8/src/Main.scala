import scala.io.Source

object Main {

  def intialMapping(signals: List[String]): List[(String, Option[Int])] = {
    signals.map(s =>
      s.length() match {
        case 2 => (s, Some(1))
        case 3 => (s, Some(7))
        case 4 => (s, Some(4))
        case 7 => (s, Some(8))
        case _ => (s, None)
      }
    )
  }

  def in(chars: List[Char], str: String): Boolean =
    chars.map(_.toString).forall(str.contains)

  case class Wiring(
      upper: Char,
      uhLeft: Char,
      uhRight: Char,
      middle: Char,
      lhLeft: Char,
      lhRight: Char,
      lower: Char
  ) {
    def check(signal: String, value: Int) = {
      value match {
        case 0 =>
          signal.size == 6 && in(
            List(upper, uhLeft, uhRight, lhLeft, lhRight, lower),
            signal
          )
        case 1 =>
          signal.size == 2 && in(List(uhRight, lhRight), signal)
        case 2 =>
          signal.size == 5 && in(
            List(upper, uhRight, middle, lhLeft, lower),
            signal
          )
        case 3 =>
          signal.size == 5 && in(
            List(upper, uhRight, middle, lhRight, lower),
            signal
          )
        case 4 =>
          signal.size == 4 && in(
            List(uhRight, uhLeft, middle, lhRight),
            signal
          )
        case 5 =>
          signal.size == 5 && in(
            List(upper, uhLeft, middle, lhRight, lower),
            signal
          )
        case 6 =>
          signal.size == 6 && in(
            List(upper, uhLeft, middle, lhLeft, lhRight, lower),
            signal
          )
        case 7 =>
          signal.size == 3 && in(List(upper, uhRight, lhRight), signal)
        case 8 =>
          signal.size == 7 && in(
            List(upper, uhLeft, uhRight, middle, lhLeft, lhRight, lower),
            signal
          )
        case 9 =>
          signal.size == 6 && in(
            List(upper, uhLeft, uhRight, middle, lhRight, lower),
            signal
          )
      }
    }
  }

  object Wiring {
    def from(mapping: Map[Int, Set[Char]]): Option[Wiring] = {
      val upper   = mapping(7) -- mapping(1)
      val uhLeft  = mapping(9) -- mapping(3)
      val uhRight = mapping(8) -- mapping(6)
      val middle  = mapping(8) -- mapping(0)
      val lhLeft  = mapping(8) -- mapping(9)
      val lhRight = mapping(3) -- mapping(2)
      val lower   = mapping(3) -- mapping(4) -- mapping(7)

      val wiringList =
        List(upper, uhLeft, uhRight, middle, lhLeft, lhRight, lower)

      if (wiringList.forall(_.size == 1))
        Some(
          Wiring(
            upper.head,
            uhLeft.head,
            uhRight.head,
            middle.head,
            lhLeft.head,
            lhRight.head,
            lower.head
          )
        )
      else None
    }
  }

  def wiring(initial: List[(String, Option[Int])]) = {
    val known   = initial.collect { case (s, Some(v)) => (v, s) }
    val unknown = initial.collect { case (s, None) => s }

    List(0, 2, 3, 5, 6, 9).permutations
      .map { p =>
        val z = p
          .filter { v => Set(0, 6, 9).contains(v) }
          .zip(unknown.filter(_.size == 6)) ++
          p.filter { v => Set(2, 3, 5).contains(v) }
            .zip(unknown.filter(_.size == 5)) ++
          known

        (
          z,
          Wiring.from(z.map { case (n, s) =>
            (n, s.toCharArray().toSet)
          }.toMap)
        )
      }
      .collect { case (s, Some(w)) =>
        (s, w)
      }
      .find { case (s, w) =>
        s.forall { case (v, s) => w.check(s, v) }
      }
  }

  def decodeValue(value: List[String], mapping: List[(Int, String)]): Int = {
    val m = mapping.map { case (v, s) => (s.toCharArray().toSet, v) }.toMap
    value.map(v => m(v.toCharArray().toSet)).map(_.toString).mkString.toInt
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList

    val signalsValues = input.map { s =>
      val tmp = s.split('|').map(_.trim)
      (tmp(0).split(' ').toList, tmp(1).split(' ').toList)
    }

    val part1 = signalsValues
      .map(_._2)
      .flatMap(intialMapping)
      .map(_._2)
      .count(_.isDefined)
    println(s"part1 ${part1}")

    val part2 = signalsValues.map { case (signal, value) =>
      val (w, _) = wiring(intialMapping(signal)).get
      decodeValue(value, w)
    }.sum
    println(s"part2 ${part2}")
  }
}
