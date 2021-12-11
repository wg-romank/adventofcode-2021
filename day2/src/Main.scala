import scala.io.Source
import scala.annotation.tailrec

object Main {

  @tailrec
  def followCourse(instructions: List[String], position: Int = 0, depth: Int = 0, aim: Option[Int] = None): Int =
    instructions match {
      case head :: tail => {
        head.split(" ").toList match {
          case direction :: amount :: Nil if amount.forall(_.isDigit) =>
            val x = amount.toInt
            direction match {
              case "forward" => followCourse(tail, position + x, depth + aim.map(_ * x).getOrElse(0), aim)
              case "down" => followCourse(tail, position, depth + x, aim.map(_ + x))
              case "up" => followCourse(tail, position, depth - x, aim.map(_ - x))
            }
          case otherwise =>
            println(s"Malformed instruction ${head}")
            position * depth
        }
      }
      case Nil => position * depth
    }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().filter(_.nonEmpty).toList

    val part1 = followCourse(input)
    println(s"part1 ${part1}")

    val part2 = followCourse(input, aim = Some(0))
    println(s"part2 ${part2 - part1}")
  }
}
