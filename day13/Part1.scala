import scala.io.Source._

object Part1 { 
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

  def isReflection(lines: Array[String], reflInd: Int): Boolean = {
    var left: Int = reflInd - 1
    var right: Int = reflInd 
    while (left >= 0 && right < lines.size) {
      if (lines(left) != lines(right)) { 
        return false
      }
      left -= 1
      right += 1
    }
    return true
  }
}
