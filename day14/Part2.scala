import scala.io.Source._

object Part2 {

  def main(args: Array[String]): Int = {
    val transformed: Array[Array[Char]] = fromFile(args(0)).getLines
      .toArray
      .map(str => str.toArray)
    var t2 = transformed
    var hashes: Array[(String, Int)] = Array()
    for (n <- 0 to 1000) {
      val s: Int = t2.zipWithIndex.map((line, j) => 
            line.zipWithIndex
              .filter((c,i) => c == 'O')
              .map((c,i) => transformed.size - j)
              .sum).sum
      val hash: String =  "" + t2.map(r=>r.mkString("")).mkString(":").hashCode
      if (hashes.contains((hash,s))) {
        val ind = hashes.indexOf((hash,s))
        val size = hashes.size
        println(hashes(ind + ((1000000000 - ind) % (size - ind)))(1))
        return 0
      }
      hashes = hashes :+ (hash, s)
      t2 = cycle(t2)
    }
    1
  }

  def cycle(lines: Array[Array[Char]]): Array[Array[Char]] = {
    lines
      .transpose
      .map(rotateAndSlide)
      .transpose
      .map(rotateAndSlide)
      .transpose
      .map(rotateAndSlide)
      .transpose
      .map(rotateAndSlide)
  }

  def rotateAndSlide(row: Array[Char]): Array[Char] = {
    row
      .mkString("")
      .reverse
      .split('#')
      .map(s => s.sorted)
      .reduce((a,b) => a + "#" + b)
      .padTo(row.size, '#')
      .toArray
  }
}
