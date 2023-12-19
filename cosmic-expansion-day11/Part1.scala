import scala.io.Source._
import scala.math._

object Part1 {
  def main(args: Array[String]) = {
    val board: Array[String] = fromFile(args(0)).getLines.toArray
    
    var galaxies: Array[(Int, Int)] = Array()
    for (y <- 0 until board.size) {
      for (x <- 0 until board(y).size) {
        if (board(y)(x) == '#') { 
          galaxies = galaxies :+ (y, x)
        }
      }
    }
    var sum = 0
    for (i <- 0 until galaxies.size-1) {
      for (j <- (i+1) until galaxies.size) { 
        sum += (abs(galaxies(i)(0) - galaxies(j)(0)) + abs(galaxies(i)(1) - galaxies(j)(1))).toInt
      }
    }
    println(sum)
  }
}
