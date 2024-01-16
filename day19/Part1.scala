import scala.io.Source._
import Category._

enum Category:
  case X
  case M
  case A
  case S

class Workflow(val name: String, val filters: Array[Filter], val terminal: String)

class Filter(val cat: Category, val predicate: (Int => Boolean), val destination: String) {
  def accepts(piece: Piece): Boolean =  predicate(piece.get(cat))
}

class Piece(val x: Int, val m: Int, val a: Int, val s: Int) {
  def get(cat: Category): Int = cat match
    case X => this.x
    case M => this.m
    case A => this.a
    case S => this.s
}

object Part1 { 
  def isAccepted(start: String, workflows: Map[String, Workflow], piece: Piece): Boolean = {
    start match
      case "A" => true
      case "R" => false
      case _ => {
      val result: String = workflows(start)
        .filters
        .find(f => f.accepts(piece))
        .map(f => f.destination)
        .getOrElse(workflows(start).terminal)
      isAccepted(result, workflows, piece)
    }
  }

  def main(args: Array[String]) = { 
    val (plines, wlines) = fromFile(args(0)).getLines
      .filter(line => !line.trim.isEmpty)
      .partition(line => line.startsWith("{"))
  
    val pieces: Array[Piece] = plines.map(line => {
      val values: Array[Int] = line
        .substring(1, line.size - 1)
        .split(",")
        .map(v => Integer.parseInt(v.split("=")(1)))
        .toArray
      Piece(values(0), values(1), values(2), values(3))
    }).toArray

    val workflows = wlines.map(line => {
      val (name, rest) = (line.split("\\{")(0), line.split("\\{")(1))
      val ff: Array[String] = rest
        .substring(0, rest.size - 1)
        .split(",")

      val fs = ff.init
      val t = ff.last
      name -> Workflow(name, fs.map(toFilter), t)
    }).toMap


    println(pieces.filter(p => isAccepted("in", workflows, p))
      .map(p => p.x + p.m + p.a + p.s)
      .sum)
  }

  def toFilter(f: String): Filter = {
    val cat: Category = Category.valueOf(f(0).toUpper + "")
    val gt: Boolean = f(1) == '>'
    val bound: Int = Integer.parseInt(f.substring(2, f.indexOf(":")))
    val dest: String = f.substring(f.indexOf(":")+1)
    Filter(cat, (x => if (gt) x > bound else x < bound), dest)
  }

}
