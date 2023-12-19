import scala.io.Source._
import scala.math._


class Hold(val number: Int,
           val lineNumber: Int,
           val xMin: Int,
           val xMax: Int) {}

object Part1 {
  var l: Int = 0
  var n: Int = 0
  var board: Array[Array[Char]] = Array()

  var info: Array[Hold] = Array()

  def main(args: Array[String]) = { 
    fromFile("input.txt").getLines.foreach(line => {
      var num: Int = 0
      if (l==0 ) {l = line.length} 
      board = board :+ Array()
      for (i <- 0 until line.length) {
        val c: Char = line.charAt(i)
        board(n) = board(n) :+ (c)
        if (c - '0' < 10 && c - '0' >= 0) {
          num = 10*(num) + (c-'0')
        }
        else {
          if (num > 0) {
            val numDigits = floor(log10(num)).toInt + 1
            val tuple: (Int,Int) = adjacencyRange(num, i - numDigits, numDigits)
            info = info :+ Hold(num, n, tuple(0), tuple(0) + tuple(1))
            num = 0
          }
        }
      }
      if (num > 0) {
        val numDigits = floor(log10(num)).toInt + 1
        val tuple: (Int,Int) = adjacencyRange(num, l - numDigits, numDigits)
        info = info :+ Hold(num, n, tuple(0), tuple(0) + tuple(1))
        num = 0
      }
      n+=1
    })
    println(info.map(hold => {
      val yMin: Int = hold.lineNumber match
        case 0 => 0
        case x => x - 1
      val yMax: Int = hold.lineNumber + 1

      var v: Int = 0
      for (x <- hold.xMin to hold.xMax) {
        for (y <- yMin to yMax) {
            if (isSymbol(x,y)) {
              v = hold.number
            }
        }
      }
      v
    }).sum)
  }

  def adjacencyRange(num: Int, start: Int, length: Int): (Int, Int) = {
    (start match 
      case i if i <= 0 => 0
      case i => i-1,
    length match
      case i if i >= l => l - 1
      case i => i + 1)
  }

  def isSymbol(x: Int, y: Int) = {
    var ry: Int = y
    var rx: Int = x
    if (x >= l) {
      rx = l - 1
    }
    if (y >= n) {
      ry = n - 1
    }
    val c: Char = board(ry)(rx)
    c match
      case '.' => false
      case digit if digit - '0' < 10 && digit - '0' >= 0 => false
      case _ => true
  }
}
