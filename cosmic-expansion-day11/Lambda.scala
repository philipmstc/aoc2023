import scala.io.Source._

object Lambda {
  def getEmpties(board: Array[String]): (Array[Int], Array[Int]) = { 
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
    (emptyRows, emptyCols)
  }
}
