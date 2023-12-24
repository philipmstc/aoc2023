import scala.io.Source._
import scala.math._

class Grid() {
  var board: Array[Array[Char]] = Array()
  def this(lines: Iterator[String]) = {
    this()
    var maxLength: Int = 0
    lines.foreach(line => {
      if (line.length > maxLength) {
        maxLength = line.length;
      }
      
      this.board = this.board :+ Array('.')
      val n: Int = this.board.size - 1
      for (i <- 0 until line.length) {
        val c: Char = line charAt i
        this.board(n) = this.board(n) :+ (c)
      }
      this.board(n) = this.board(n) :+ '.'
    })
    var empty: Array[Char] = Array()
    for (i <- 0 until maxLength + 2) {
      empty = empty :+ '.'
    }
    this.board = empty +: this.board :+ empty
  }
  def getBoard(): Array[Array[Char]] = {
    this.board
  }

  override def toString: String = { 
    var sb: String = ""
    this.board.foreach(
      row => {
        sb = sb + "\n"
        row.foreach(
          col => {
            sb = sb + col
          })
      })
    sb
  }
}

object Part2 {
  


  def main(Args: Array[String]) = {
    val grid = Grid(fromFile("input.txt").getLines)
    var gears: Array[(Int, Int)] = Array()
    for (y <- 0 until grid.board.size) { 
      for (x <- 0 until grid.board(y).size) { 
        if (grid.board(y)(x) == '*') {
          gears = gears :+ (x,y)
        }
      }
    }
    for (y <- 0 until grid.board.size) { 
      for (x <- 0 until grid.board(y).size) { 
        if (gears contains (x,y)) {
          for (xAdj <- x-1 until x +1 ) {
            for (yAdj <- y-1 until y + 1) {
            }
          }
        }
      }
    }
    println(grid)
    gears.foreach(println)
  }

}
