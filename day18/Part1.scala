import scala.io.Source._
import scala.math._
import Dir._
class Direction(val dir: Dir, val count: Int, val colorCode: String)

class Trench(val cubes: Array[Cube]) {
  def draw(): Int = {
    var volume: Int = 0
    var pvolume: Int = 0
    var lowestX: Int = this.cubes.map(cube =>cube.x).min
    var lowestY: Int = this.cubes.map(cube =>cube.y).min

    val tcubes = this.cubes.map(cube => Cube(cube.x - lowestX, cube.y - lowestY, cube.color, cube.insideDir))
    var includingInside: Array[Cube] = tcubes

    // gotta account for duplicate areas counting

    tcubes.foreach(cube => {
        pvolume += 1 
        volume += 1
        val nextCubes: Array[Cube] = ((cube.insideDir match
        case L => (tcubes.filter(qube => qube.y == cube.y && qube.x < cube.x).map(qb => qb.x).max until cube.x)
        case R => (cube.x until tcubes.filter(qube => qube.y == cube.y && qube.x > cube.x).map(qb => qb.x).min)
        case U => (tcubes.filter(qube => qube.x == cube.x && qube.y < cube.y).map(qb => qb.y).max until cube.y)
        case D => (cube.y until tcubes.filter(qube => qube.x == cube.x && qube.y > cube.y).map(qb => qb.y).min)
        ).map(n => 
              cube.insideDir match
                case L|R => Cube(n, cube.y, cube.color, cube.insideDir)
                case U|D => Cube(cube.x, n, cube.color, cube.insideDir)
              ).toArray)
        includingInside = includingInside.appendedAll(nextCubes)
      })
    includingInside.groupBy(cube => (cube.x, cube.y)).size
  }
}

class Cube(val x: Int, val y: Int, val color: String, var insideDir: Dir)

enum Dir(val xOff: Int, val yOff: Int):
  case U extends Dir(0,-1)
  case R extends Dir(1, 0)
  case D extends Dir(0, 1)
  case L extends Dir(-1,0)


object Part1 { 
  def insideDir(cc: Boolean, current: Dir): Dir = { 
    current match
      case U => if (cc) R else L
      case D => if (cc) L else R
      case L => if (cc) U else D
      case R => if (cc) D else U
  }

  def main(args: Array[String]) = { 
    val directions: Array[Direction] = fromFile(args(0))
      .getLines
      .map(toDirection)
      .toArray
    val trench: Trench = digTrench(directions)   
    println(trench.draw())
  }

  def toDirection(line: String): Direction = {
    val dir: String = line.split(" ")(0)
    val count: Int = Integer.parseInt(line.split(" ")(1))
    val temp: String = line.split(" ")(2)
    val colorCode: String = temp.substring(1, temp.size - 1)
    Direction(Dir.valueOf(dir), count, colorCode)
  }

  def digTrench(directions: Array[Direction]): Trench = { 
    var x: Int = 0
    var y: Int = 0
    var dir: Dir = null
    val d1: Dir = directions(0).dir
    val cc: Boolean = directions(directions.size-1).dir match 
      case U => d1 == R
      case D => d1 == L
      case L => d1 == U
      case R => d1 == D
    val cubes: Array[Cube] = directions.zipWithIndex.map((direction, k) =>
    {
      val range: Range = (direction.dir match 
        case Dir.U => (y-1 until y-(direction.count+1) by -1)
        case Dir.R => (x+1 until x+direction.count+1)
        case Dir.D => (y+1 until y+direction.count+1)
        case Dir.L => (x-1 until x-(direction.count+1) by -1)
      )
      range.map(n => { 
        direction.dir match 
          case Dir.U | Dir.D => y = n
          case Dir.L | Dir.R => x = n
       
        val nextInsideDir = insideDir(!cc, direction.dir)
        Cube(x,y, direction.colorCode, nextInsideDir) 
      }).toArray
    }).flatten
    Trench(cubes)
  }
}
