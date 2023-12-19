import scala.io.Source._

object Part2
{
  val numbers = Array("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def main(args: Array[String]) = {
    val lines = fromFile("input.txt").getLines
    println(lines.map(find2digits).sum)
  }

  def find2digits(line: String): Int = {
    var left = -1 
    var right = -1
    var lbuff = ""
    var rbuff = ""

    for (i <- 0 until line.length) {
      numbers.foreach(n => {
        if (lbuff contains n) {
          left = numbers indexOf n
        }
        if (rbuff contains n) { 
          right = numbers indexOf n
        }
      })
      
      if (left == -1) {
        find(line, i) match
          case Left(num) => left = num
          case Right(buff) => lbuff = lbuff + buff
      }
      if (right == -1) {
        find(line, line.length - i - 1) match
          case Left(num) => right = num
          case Right(buff) => rbuff = buff + rbuff
      }
      if (left != -1 && right != -1) {
        return 10*left + right;
      }
    }
    return 10*left + right; 
  }

  def find(line: String, i: Int): Either[Int,String] = {
    val c:Char = line charAt i
    if (isNumber(c)) {
      return Left(c.toInt - 48)
    }
    else { 
      Right(c.toString)
    }
  }

  def isNumber(c: Char): Boolean = {
    c >= 48 && c <= 57
  }
}
