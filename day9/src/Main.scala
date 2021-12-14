import scala.io.Source

object Main {

  case class HeightMap(x: List[List[Byte]]) {
    val h = x.length
    val w = x.head.length

    def neightboorIndicies(r: Int, c: Int): List[(Int, Int)] =
      (if (r > 0) List((r - 1, c)) else List()) ++
      (if (r < h - 1) List((r + 1, c)) else List()) ++
      (if (c > 0) List((r, c - 1)) else List()) ++
      (if (c < w - 1) List((r, c + 1)) else List())

    def neightboors(r: Int, c: Int): List[Byte] =
      neightboorIndicies(r, c).map { case (r, c) => x(r)(c) }

    def lowPointsCoordinates: List[(Int, Int)] = 
      x.zipWithIndex.flatMap {
        case (r, rId) => r.zipWithIndex.flatMap {
          case (v, cId) =>
            val p = x(rId)(cId)
            if (neightboors(rId, cId).forall(_ > p)) List((rId, cId)) else List()
        }
      }

    def lowPoints: List[Byte] =
      lowPointsCoordinates.map { case (r, c) => x(r)(c) }

    override def toString: String =
      x.map(r => r.mkString).mkString("\n")

    def basinsView: String =
      x.map(r => r.map(v => if (v == 9) '*' else '.').mkString).mkString("\n")

    def basinSize(toExplore: List[(Int, Int)], beenTo: Set[(Int, Int)] = Set.empty): Int = {
      if (toExplore.isEmpty) beenTo.size
      else {
        val nextExpore = toExplore
          .flatMap { case (r, c) => neightboorIndicies(r, c) }
          .filter { case (r, c) => x(r)(c) != 9 && !beenTo.contains((r, c)) }
        
        basinSize(nextExpore, beenTo ++ toExplore.toSet)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList

    val hm = HeightMap(input.map(_.split("").map(_.toByte).toList))

    val part1 = hm.lowPoints.map(_ + 1).sum
    println(s"part1 ${part1}")

    val part2 = hm.lowPointsCoordinates.map {
      lp => hm.basinSize(List(lp))
    }.sorted.takeRight(3).product
    println(s"part2 ${part2}")
  }
}
