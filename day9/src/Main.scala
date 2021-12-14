import scala.io.Source

object Main {

  case class HeightMap(x: List[List[Byte]]) {
    val h = x.length
    val w = x.head.length

    def neightboors(r: Int, c: Int): List[Byte] = {
      val ni =
        (if (r > 0) List((r - 1, c)) else List()) ++
        (if (r < h - 1) List((r + 1, c)) else List()) ++
        (if (c > 0) List((r, c - 1)) else List()) ++
        (if (c < w - 1) List((r, c + 1)) else List())

      ni.map { case (r, c) => x(r)(c) }
    }

    def lowPoints: List[Byte] =
      x.zipWithIndex.flatMap {
        case (r, rId) => r.zipWithIndex.flatMap {
          case (v, cId) =>
            val p = x(rId)(cId)
            if (neightboors(rId, cId).forall(_ > p)) List(p) else List()
        }
      }

    override def toString: String =
      x.map(r => r.mkString).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList

    val hm = HeightMap(input.map(_.split("").map(_.toByte).toList))

    val part1 = hm.lowPoints.map(_ + 1).sum
    println(s"part1 ${part1}")
  }
}
