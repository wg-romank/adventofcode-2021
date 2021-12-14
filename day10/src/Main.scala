import scala.io.Source

object Main {

  def matching(bracket: Char): Char =
    bracket match {
      case '[' => ']'
      case '{' => '}'
      case '(' => ')'
      case '<' => '>'
    }

  def score(char: Char): Int =
    char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }

  def autocompleteScore(chars: List[Char]): Long =
    chars.foldLeft(0L) {
      case (acc, next) =>
        acc * 5L + (next match {
          case ')' => 1L
          case ']' => 2L
          case '}' => 3L
          case '>' => 4L
        })
    }

  def compile(line: List[Char], open: List[Char] = List.empty): Either[Int, List[Char]] =
    line match {
      case head :: tail =>
        head match {
          case '{' | '[' | '(' | '<' =>
            compile(tail, head +: open)
          case '}' | ']' | ')' | '>' =>
            if (matching(open.head) == head)
              compile(tail, open.tail)
            else Left(score(head))
        }
      case Nil => Right(open.map(matching))
    }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input").getLines().toList.map(_.toCharArray().toList)

    val part1 = input.map(l => compile(l)).collect { case Left(score) => score }.sum
    println(s"part1 ${part1}")

    val acScores = input.map(l => compile(l)).collect { case Right(v) => autocompleteScore(v) }.sorted
    println(acScores)
    val part2 = acScores(acScores.length / 2)
    println(s"part2 ${part2}")
  }
}
