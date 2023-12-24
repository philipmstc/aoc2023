import scala.io.Source._
import scala.math._

object Part2 { 
  def main(args: Array[String]) = {
    val allLines: Array[String] = fromFile(args(0)).getLines.toArray
    var part: Array[String] = Array()
    var sum = 0
    for (i <- 0 until allLines.size)
    {
      if (i == allLines.size-1) {
        part = part :+ allLines(i)
      }
      if (allLines(i).isEmpty || i == allLines.size-1)
      {
        val (rows, cols) = parse(part)
        val rowReflection = findReflection(rows)
        val colReflection = findReflection(cols)
        sum += colReflection + (100*rowReflection)
        part = Array()
      }
      else part = part :+ allLines(i)
    }
    println(sum)
  }
   
  def parse(lines: Array[String]): (Array[String], Array[String]) = {
    var rows: Array[String] = Array()
    var cols: Array[String] = Array()

    rows = lines.toArray
    val length = lines(0).size
    for (i <- 0 until length) {
      cols = cols :+ String(lines.map(l => l(i)))
    }
    (rows, cols)
  }

  def findReflection(lines: Array[String]) : Int = {
    bruteForce(lines)  
  }

  def bruteForce(lines: Array[String]): Int = { 
    for (reflInd <- 1 until lines.size) {
      if (isReflection(lines, reflInd)) { 
        return reflInd
      }
    }
    0
  }

  def isOffByOne(l1: String, l2: String): Boolean = {
    val b1: Int = Integer.parseInt(l1, 2)
    val b2: Int = Integer.parseInt(l2, 2)

    val xor = b1 ^ b2
    val log2 = log(xor)/log(2)
    log2.toInt - log2 == 0
  }

  def toBin(line: String): String = { 
    line.map(c => c match 
      case '#' => '1'
      case '.' => '0')
  }

  def isReflection(lines: Array[String], reflInd: Int): Boolean = {
    var left: Int = reflInd - 1
    var right: Int = reflInd 
    var smudgeCleared = 0
    while (left >= 0 && right < lines.size) {
      val bleft = toBin(lines(left))
      val bright = toBin(lines(right))
      if (bleft != bright) { 
        if (smudgeCleared > 0) {
          return false
        }
        else if (isOffByOne(bleft,bright)) {
          smudgeCleared += 1
        }
        else { 
          return false
        }
      }
      left -= 1
      right += 1
    }
    return smudgeCleared == 1
  }
}
