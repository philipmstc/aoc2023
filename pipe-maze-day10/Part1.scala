import scala.io.Source._

class Pipe(val north: Array[Char],
           val east: Array[Char], 
           val south: Array[Char],
           val west: Array[Char])

val pipes: Map[Char, Array[Array[Char]]] = Map(
  'S' -> Array(Array('|', '7', 'F'), Array('-', 'J', '7'), Array('|', 'L', 'J'), Array('-', 'L', 'F')),
  '|' -> Array(Array('|', '7', 'F'), Array[Char](), Array('|', 'L', 'J'), Array[Char]()),
  '-' -> Array(Array[Char](), Array('-', 'J', '7'), Array[Char](), Array('-', 'L', 'F')),
  'L' -> Array(Array('|', '7', 'F'), Array('-', 'J', '7'), Array[Char](), Array[Char]()),
  'J' -> Array(Array('|', '7', 'F'), Array[Char](), Array[Char](), Array('-', 'L', 'F')),
  '7' -> Array(Array[Char](), Array[Char](), Array('|', 'L', 'J'), Array('-', 'L', 'F')),
  'F' -> Array(Array[Char](), Array('-', 'J', '7'), Array('|', 'L', 'J'), Array[Char]())
)


object Part1 { 
  def main(args: Array[String]) = { 
    
    var globalMax: Int = 0
    var localMax: Int = -1
    val lines: Array[String] = fromFile(args(0)).getLines.toArray
    val board: Array[Array[Int]] = lines.map(l => l.map(c => {
      c match 
        case 'S' => 1 
        case _ => 0
      }).toArray
    ).toArray
    var target: Char = 'S'
    while (localMax != globalMax) {        
      localMax = globalMax
      for(y <- 0 until lines.size) {
        for(x <- 0 until lines(y).length) {
          if (board(y)(x) > 0) {
            if (board(y)(x) > globalMax) {
              globalMax = board(y)(x)
            }
            val adjacents: Array[Char] = 
              Array(north(board, lines, x,y), east(board, lines, x,y), south(board, lines, x,y), west(board, lines,x,y))
            adjacents.zipWithIndex.filter { e => isAdjacent(lines(y)(x), e._1, e._2) }.foreach {e =>
              e._2 match 
                case 0 => board(y-1)(x) = board(y)(x) + 1 
                case 1 => board(y)(x+1) = board(y)(x) + 1
                case 2 => board(y+1)(x) = board(y)(x) + 1
                case 3 => board(y)(x-1) = board(y)(x) + 1
            }
          }
        }
      }
    }
    println(globalMax - 1)
  }

  def isAdjacent(cell: Char, other: Char, dir: Int): Boolean = { 
     pipes(cell)(dir).contains(other)
  }

  def north(vals: Array[Array[Int]], board: Array[String], x: Int, y: Int): Char = { 
    y match 
      case 0 => '.'
      case y if vals(y-1)(x) > 0 => '.'
      case y => board(y-1)(x)
  }
  def east(vals: Array[Array[Int]],board: Array[String], x: Int, y: Int): Char = { 
    x match
      case x if x == board(y).size - 1 => '.'
      case x if vals(y)(x + 1) > 0 => '.'
      case x => board(y)(x + 1)
  } 
  def south(vals: Array[Array[Int]],board: Array[String], x: Int, y: Int): Char = { 
    y match 
      case y if y == board(y).size - 1 => '.'
      case y if vals(y+1)(x) > 0 => '.'
      case y => board(y+1)(x)
  }
  def west(vals: Array[Array[Int]],board: Array[String], x: Int, y: Int): Char = { 
    x match 
      case 0 => '.'
      case x if vals(y)(x-1) > 0 => '.'
      case x => board(y)(x-1)
  }
}
