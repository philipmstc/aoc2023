import scala.io.Source._
import scala.math._


object Part1 { 
  def main(args: Array[String]) = { 
    val lines: Array[Array[Int]] = fromFile(args(0)).getLines.map(toEntry).toArray
    val times: Array[Int] = lines(0)
    val dists: Array[Int] = lines(1)

    var total: Int = 1
    for (i <- 0 until times.size) {
      total *= minTimeHeld(times(i), dists(i))
    }
    println(total)
  }

   def minTimeHeld(t: Int, d: Int): Int = { 
    val r1: Double = (t + sqrt(pow(t,2) - 4*d)) / 2
    val r2: Double = (t - sqrt(pow(t,2) - 4*d)) / 2
    val hi: Double = r1 > r2 match 
      case true => r1 
      case false => r2
    val lo: Double = r1 <=r2 match 
      case true => r1 
      case false => r2
    val diff: Double = ceil(hi) - floor(lo+1)

    floor(abs(diff)).toInt
  }

  def toEntry(line: String): Array[Int] = { 
    (line.split(":")(1).split(" ")
      .map(v => v.trim)
        .filter(v => !v.isEmpty)
        .map(v => v.toInt))
  }

}
