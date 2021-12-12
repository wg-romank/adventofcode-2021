import scala.io.Source
import scala.annotation.tailrec

object Main {

  sealed trait Number {
    def x: Int
    def marked: Boolean
  }
  case class Marked(x: Int) extends Number {
    val marked = true
  }
  case class Unmarked(x: Int) extends Number {
    val marked = false
  }

  case class Board(b: List[List[Number]]) {
    def withRow(line: String): Board = {
      copy(b =
        b :+ line
          .split(" ")
          .filter(_.nonEmpty)
          .map(_.toInt)
          .map(Unmarked.apply)
          .toList
      )
    }

    def call(number: Int): Board =
      copy(b = b.map(r => r.map(n => if (n.x == number) Marked(n.x) else n)))

    def isWinning: Boolean = {
      val winningRow = b.exists(r => r.forall(_.marked))
      val winningCol = b.flatMap(r => r.zipWithIndex).groupBy(_._2).exists {
        case (_, col) => col.forall(_._1.marked)
      }

      winningRow || winningCol
    }

    def sumUnmarked: Int =
      b.map(r => r.collect { case Unmarked(x) => x }.sum).sum

    override def toString: String =
      b.map(r =>
        r.map {
          case Marked(x)   => s"[${x}]"
          case Unmarked(x) => x
        }.mkString(" ")
      ).mkString("\n")
  }

  object Board {
    def empty: Board = new Board(List.empty[List[Number]])
  }

  @tailrec
  def parseBoards(
      lines: List[String],
      boards: List[Board] = List(),
      currentBoard: Option[Board] = None
  ): List[Board] =
    lines match {
      case head :: next =>
        currentBoard match {
          case Some(b) =>
            if (head.nonEmpty)
              parseBoards(next, boards, Some(b.withRow(head)))
            else
              parseBoards(next, boards :+ b, None)
          case None =>
            if (head.nonEmpty)
              parseBoards(next, boards, Some(Board.empty.withRow(head)))
            else
              parseBoards(next, boards, None)
        }
      case Nil => boards ++ currentBoard.toList
    }

  def parseInput(lines: List[String]): (List[Int], List[Board]) =
    lines match {
      case head :: next =>
        val calls  = head.split(",").map(_.toInt).toList
        val boards = parseBoards(next)
        (calls, boards)
      case Nil =>
        println(s"malformed input\n${lines.mkString("\n")}")
        ???
    }

  def playPart1(calls: List[Int], boards: List[Board]): Int =
    calls match {
      case head :: next =>
        val updatedBoards = boards.map(_.call(head))
        updatedBoards.find(_.isWinning) match {
          case Some(b) => b.sumUnmarked * head
          case None    => playPart1(next, updatedBoards)
        }

      case Nil =>
        println("we are out of numbers, but no winners")
        ???
    }

  def playPart2(
      calls: List[Int],
      boards: List[Board],
      callIdx: Int = 0
  ): Int = {
    val call          = calls(callIdx)
    val nextIdx       = (callIdx + 1) % (calls.length - 1)
    val updatedBoards = boards.map(_.call(call))
    updatedBoards match {
      case head :: Nil if head.isWinning=> head.sumUnmarked * call
      case otherwise => playPart2(calls, updatedBoards.filter(!_.isWinning), nextIdx)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList

    val (calls, boards) = parseInput(input)

    val part1 = playPart1(calls, boards)
    println(s"part1 ${part1}")

    val part2 = playPart2(calls, boards)
    println(s"part2 ${part2}")
  }
}
