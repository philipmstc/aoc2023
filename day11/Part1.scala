import scala.io.Source._
import scala.math._
import Lambda.getEmpties

object Part1 {
  def main(args: Array[String]) = {
    val board: Array[String] = fromFile(args(0)).getLines.toArray
    val expansionRate: Int = args(1).toInt
    var galaxies: Array[(Int, Int)] = Array()
    for (y <- 0 until board.size) {
      for (x <- 0 until board(y).size) {
        if (board(y)(x) == '#') { 
          galaxies = galaxies :+ (y, x)
        }
      }
    }

    var sum: Long = 0
    val (emptyRows, emptyCols) = getEmpties(board)
    for (i <- 0 until galaxies.size-1) {
      for (j <- (i+1) until galaxies.size) { 
        val highRow: Int = max(galaxies(i)(0), galaxies(j)(0)).toInt
        val highCol: Int = max(galaxies(i)(1), galaxies(j)(1)).toInt
        val lowRow: Int = min(galaxies(i)(0), galaxies(j)(0)).toInt
        val lowCol: Int = min(galaxies(i)(1), galaxies(j)(1)).toInt

        val spanningEmptyRows: Int = emptyRows.filter(r=> r > lowRow && r < highRow).size
        val spanningEmptyCols: Int = emptyCols.filter(c=> c > lowCol && c < highCol).size

        val rowDistance: Long = (highRow - lowRow - spanningEmptyRows) + (spanningEmptyRows * expansionRate)
        val colDistance: Long = (highCol - lowCol - spanningEmptyCols) + (spanningEmptyCols * expansionRate)
        sum += rowDistance + colDistance
 
      }
    }
    println(sum)
  }
}
