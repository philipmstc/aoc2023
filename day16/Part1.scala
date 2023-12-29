import scala.io.Source._

enum Dir: 
  case N,S,E,W

class Tile(val x: Int, val y: Int, var visitedFrom: Set[Dir])
class Pointer(var x: Int, var y: Int, var dir: Dir, var alive: Boolean)

object Part1 {
  def main(args: Array[String]) = {
    var pointers: Array[Pointer] = Array(Pointer(-1,0,Dir.E, true))
    val board: Array[Array[Char]] = fromFile(args(0)).getLines.map(s => s.toCharArray).toArray
    var loop: Boolean = true
    var tiles: Array[Tile] = board.zipWithIndex.flatMap((sub, i) => 
        sub.zipWithIndex.map((c, j) => Tile(j, i, Set()))).toArray

    while (!pointers.filter(p=>p.alive).isEmpty) {
      var tpointers = pointers
      pointers.filter(p=>p.alive).foreach(p => 
      {
        val (px, py) = (p.x, p.y)
        val t: Tile = tiles.find(cand => cand.x == px && cand.y == py).getOrElse(Tile(px, py, Set()))
        if (t.visitedFrom.contains(p.dir)) {
          p.alive = false
        }
        else {
          t.visitedFrom += p.dir
          val (tx, ty) = p.dir match    
            case Dir.N => (px, py - 1)
            case Dir.S => (px, py + 1)
            case Dir.E => (px + 1, py)
            case Dir.W => (px - 1, py)
          p.x = tx
          p.y = ty

          if (p.x < 0 || p.x >= board(0).size || p.y < 0 || p.y >= board.size) {
            p.alive = false
          }
          else {
            val next: Char = board(p.y)(p.x)
            if (next == '|') {
              if (p.dir == Dir.E || p.dir == Dir.W) {
                p.dir = Dir.N
                tpointers = tpointers :+ Pointer(p.x, p.y, Dir.S, true)
              }
            }
            if (next == '-') { 
              if (p.dir == Dir.S || p.dir == Dir.N) {
                p.dir = Dir.E
                tpointers = tpointers :+ Pointer(p.x, p.y, Dir.W, true)
              }
            }
            if (next == '\\') {
              p.dir = p.dir match
                case Dir.N => Dir.W
                case Dir.S => Dir.E
                case Dir.E => Dir.S
                case Dir.W => Dir.N
            }
            if (next == '/') {
              p.dir = p.dir match 
                case Dir.N => Dir.E 
                case Dir.S => Dir.W
                case Dir.E => Dir.N
                case Dir.W => Dir.S
            }
          }
        }
      })
      pointers = tpointers
    }
    println(tiles.filter(t => !t.visitedFrom.isEmpty).size)
  }
}
