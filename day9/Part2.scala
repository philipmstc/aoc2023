import scala.io.Source._

object Part2 {
  def main(args: Array[String]) = {
    println(fromFile(args(0)).getLines
      .map(l => (l.split(" "))
        .map(s=>s.toInt))
      .toArray
      .map(findPrev)
      .sum)
  }

  def findPrev(sequence: Array[Int]): Int = { 
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

    var total: Int = 0
    for (i <- 0 until result.size) {
      total = total + ((i % 2) match
        case 0 => 1
        case 1 => -1)*result(i).reverse.last
    }
    total
  }
}
