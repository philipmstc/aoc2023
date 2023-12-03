import scala.io.Source._

class Colors(val red: Int, 
             val green: Int, 
             val blue: Int) {}

object Part2 {
  def main(args: Array[String]) = {
    println(fromFile("input.txt").getLines.map(power).sum)
  }

  def power(line: String): Int = {
    val draws: Array[Colors] = line.split(":")(1).split(";").map(getColors)
    draws.map(c => c.red).reduceLeft(_ max _) * draws.map(c => c.green).reduceLeft(_ max _) * draws.map(c => c.blue).reduceLeft(_ max _)
  }

  def getColors(draw: String): Colors = {
    var r: Int = 0
    var g: Int = 0
    var b: Int = 0
    draw.split(",").map(d => d.trim.split(" ")).foreach(c => {
      c(1) match 
        case "red" => r = c(0).toInt
        case "green" => g = c(0).toInt
        case "blue" => b = c(0).toInt
    })
    Colors(r,g,b)
  }
}
