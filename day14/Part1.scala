import scala.io.Source._

object Part1 {

  def main(args: Array[String]) = {
    val transformed: Array[Array[Char]] = fromFile(args(0)).getLines
      .toArray
      .map(str => str.toArray)
      .transpose
      .map(r => cycle(r))
      
    println(transformed.map(
      line => { 
        line.zipWithIndex.map((c,i) => 
            c match 
              case 'O' => i + 1
              case _ => 0
        )
        .sum
      }).sum)
  }

  def cycle(row: Array[Char]): Array[Char] = {
    row.mkString("")
      .reverse
      .split('#')
      .map(s => s.sorted)
      .reduce((a,b) => a + "#" + b)
      .padTo(row.size, '#')
      .toArray
  }
}
