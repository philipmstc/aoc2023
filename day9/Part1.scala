import scala.io.Source._

object Part1 {
  def main(args: Array[String]) = {
    println(fromFile(args(0)).getLines
      .map(l => (l.split(" "))
        .map(s=>s.toInt))
      .toArray
      .map(findNext)
      .sum)
  }

  def findNext(sequence: Array[Int]): Int = { 
    var result: Array[Array[Int]] = Array(sequence)
    var diffs: Array[Int] = Array(1)
    var j: Int = 0
  
    while (diffs.find(x => x != 0).isDefined) {
      diffs = Array()
      for (i <- 0 until result(j).size-1) {
        diffs = diffs :+ (result(j)(i+1) - result(j)(i)) 
      }
      result = result :+ diffs
      j+=1
    }

    result.map(arr => arr.last).sum
  }
}
