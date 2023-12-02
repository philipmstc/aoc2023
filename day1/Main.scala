import scala.io.Source._

object Main
{
  // indexOf == value
  val numbers = Array(
    "zero", 
    "one", 
    "two", 
    "three", 
    "four", 
    "five", 
    "six", 
    "seven", 
    "eight", 
    "nine"
  )



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
        findLeft(line, i, lbuff) match
          case Left(num) => left = num
          case Right(buff) => lbuff = buff
      }
      if (right == -1) {
        findRight(line, i, rbuff) match
          case Left(num) => right = num
          case Right(buff) => rbuff = buff
      }
      if (left != -1 && right != -1) {
        println("left: " + left + "[" + lbuff + "], right: " + right + "[" + rbuff + "] line: " + line)
        return 10*left + right;
      }
    }

    return 10*left + right; 
  }



  def findLeft(line: String, i: Int, buff: String): Either[Int,String] = {
    var c: Char = line.charAt(i)
    if (isNumber(c)) {
      return Left(c.toInt - 48)
    }
    Right(buff + c)
  }

  def findRight(line: String, i: Int, buff: String): Either[Int, String] = {  
    var c: Char = line.charAt(line.length - i - 1)
    if (isNumber(c)) {
     return Left(c.toInt - 48)
    }
    return Right(c + buff)
  }

  def isNumber(c: Char): Boolean = {
    c >= 48 && c <= 57
  }
}
