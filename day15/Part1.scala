import scala.io.Source._

object Part1 {
  var hashmap: Array[Array[String]] = Array.fill[Array[String]](256)(Array())
  def main(args: Array[String]) = {
    val sum = fromFile(args(0)).getLines.foreach(
      l => l.split(',')
        .foreach(hash)
      )
    println(hashmap.zipWithIndex.map((box, i) => 
        box.zipWithIndex.map((lens, j) => 
            (i+1) * (j+1) * Integer.valueOf(lens.split(" ")(1))
        ).sum
    ).sum)
  }

  def hash(line: String) = {
    val adding: Boolean = line.matches(".*=[0-9]*")
    val label: String = adding match 
      case true => line.split("=")(0)
      case false => line.split("-")(0)

    var curr: Int = 0
    for (i <- 0 until label.size) {
      val c: Char = label(i)
      curr += c.toInt
      curr *= 17
      curr %= 256
    }
    if (adding) { 
      val current: Array[String] = hashmap(curr)

      val indOf: Option[Int] = hashmap(curr).zipWithIndex.find((l, i) => l.startsWith(label + " ")).map((l, i) => i)
      indOf match
        case None => hashmap(curr) = current :+ (line.replace("=", " ")).toString
        case Some(x) => hashmap(curr)(x) = line.replace("=", " ").toString
    }
    else { 
      val current: Array[String] = hashmap(curr)
      hashmap(curr) = current.filter(v => !v.split(" ")(0).equals(label))
    }
  }


}
