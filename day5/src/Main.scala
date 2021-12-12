import scala.io.Source

object Main {
  case class Location(x: Int, y: Int)

  object Location {
    def fromString(string: String): Location = {
      val tmp = string.split(",")

      new Location(tmp(0).toInt, tmp(1).toInt)
    }
  }

  def isHorizontal(from: Location, to: Location): Boolean = from.y == to.y

  def isVertial(from: Location, to: Location): Boolean = from.x == to.x

  case class OceanFloor(l: List[List[Int]]) {
    def logMeasurements(from: Location, to: Location): OceanFloor = {
      val minX = Math.min(from.x, to.x)
      val maxX = Math.max(from.x, to.x)
      val minY = Math.min(from.y, to.y)
      val maxY = Math.max(from.y, to.y)

      val lookup: (Int, Int) => Boolean = (col, row) =>
        if (isHorizontal(from, to)) row == from.y && col >= minX && col <= maxX
        else if (isVertial(from, to)) col == from.x && row >= minY && row <= maxY
        else false

      copy(l = l.zipWithIndex.map { 
        case (r, rIdx) => r.zipWithIndex.map {
          case (e, cIdx) => if (lookup(cIdx, rIdx)) e + 1 else e
        }
      })
    }

    def countOverlaps: Int = l.map(_.count(_ > 1)).sum

    override def toString: String =
      l.map(r => r.map(e => if (e == 0) '.' else e.toString).mkString(" ")).mkString("\n")
  }

  object OceanFloor {
    def of(maxX: Int, maxY: Int): OceanFloor = new OceanFloor(
      (0 to maxY).map(_ => List.fill(maxX + 1)(0)).toList
    )
  }

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input")
      .getLines()
      .map(_.split("->").map(_.trim).map(Location.fromString).toList)
      .toList
    
    val maxX = input.flatten.map(_.x).max
    val maxY = input.flatten.map(_.y).max

    val of = input.foldLeft(OceanFloor.of(maxX, maxY)) {
      case (of, input) => of.logMeasurements(input(0), input(1))
    }

    // println(of)

    val part1 = of.countOverlaps
    println(s"part1 ${part1}")
  }
}
