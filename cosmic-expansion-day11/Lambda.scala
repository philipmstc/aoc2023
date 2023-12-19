import scala.io.Source._

object Lambda {
  def main(args: Array[String]) = {
    val board: Array[Array[Char]] = fromFile(args(0)).getLines.map(str => str.toCharArray).toArray
    var emptyRows: Array[Int] = Array()
    var emptyCols: Array[Int] = Array()
    for (y <- 0 until board.size) {
      var hasGalaxiesInRow: Boolean = false
      for (x <- 0 until board(y).size) {
        if (board(y)(x) == '#') {
          hasGalaxiesInRow = true
        }
      }
      if (!hasGalaxiesInRow) {
        emptyRows = emptyRows :+ y
      }
    }

    for (x <- 0 until board.size) {
      var hasGalaxiesInCol: Boolean = false
      for (y <- 0 until board(x).size) {
        if (board(y)(x) == '#') {
          hasGalaxiesInCol = true
        }
      }
      if (!hasGalaxiesInCol) {
        emptyCols = emptyCols :+ x
      }
    }

    var sb: String = ""
    for (y <- 0 until board.size) {
      if (emptyRows.contains(y)) {
        for (x <- 0 until (board.size + emptyCols.size)) {
          sb = sb + "."
        }
        println(sb)
        sb = ""
      }
      for (x <- 0 until board.size) { 
        sb = sb + board(y)(x)
        if (emptyCols.contains(x)) {
          sb = sb + "."
        }
      }
      println(sb)
      sb = ""
    }
  }
}
