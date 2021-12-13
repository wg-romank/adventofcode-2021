import scala.io.Source

object Main {

  case class Flock(fish: Map[Long, Long]) {
    def tick: Flock = {
      val newFishes: Long = fish.get(0).getOrElse(0) 
      val updatedMap: Map[Long, Long] = fish.map {
        case (kind, amount) => (kind - 1, amount)
      }

      Flock(
        if (newFishes > 0)
          updatedMap.updated(
            8, updatedMap.get(8).map(_ + newFishes).getOrElse(newFishes)
          ).updated(
            6, updatedMap.get(6).map(_ + newFishes).getOrElse(newFishes)
          ).filter(_._1 >= 0)
        else
          updatedMap
      )
    }

    def totalFish: Long = fish.map(_._2).sum
  }

  def main(args: Array[String]): Unit = {
    val initial = Source.fromResource("input").getLines().toList.head.split(",").map(_.toLong).toList
    val f = Flock(initial.groupBy(k => k).map { case (t, l) => (t, l.length.toLong) }.toMap)


    val f80 = (0 until 80).foldLeft(f) { case (f, _) => f.tick }
    val part1 = f80.totalFish
    println(s"part1 ${part1}")

    val f256 = (0 until 256).foldLeft(f) { case (f, _) => f.tick }
    val part2 = f256.totalFish
    println(s"part2 ${part2}")
  }
}
