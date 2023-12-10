import scala.io.Source._
import scala.math._


object Part2 { 
  def main(args: Array[String]) = { 
    val lines: Array[Array[Long]] = fromFile(args(0)).getLines.map(toEntry).toArray
    val times: Array[Long] = lines(0)
    val dists: Array[Long] = lines(1)

    var total: Long = 1
    for (i <- 0 until times.size) {
      total *= minTimeHeld(times(i), dists(i))
    }
    println(total)
  }

   def minTimeHeld(t: Long, d: Long): Long = { 
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

  def toEntry(line: String): Array[Long] = { 
    Array(line.split(":")(1).replace(""" """, "").toLong)
  }

}
