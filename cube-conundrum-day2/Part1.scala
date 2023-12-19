import scala.io.Source._

object Part1 {
 def main(args: Array[String]) = {
  println(fromFile("input.txt").getLines().map(gameIdIfPossible).sum)
 }

 def gameIdIfPossible(line: String): Int = {
  val splits: Array[String] = line.split(":")
  val gameId: Int = splits(0).substring(5, splits(0).length).toInt
  val game: Array[String] = splits(1).split(";")
  
  game.exists(draws => impossible(draws)) match 
    case true => 0
    case false => gameId
 }

 def impossible(draws: String): Boolean = {
  val splits: Array[String] = draws.split(",")
  val colors: Array[Array[String]] = splits.map(draw => draw.trim.split(" "))
  colors.exists(nc => {
    nc(1) match
      case "blue" => nc(0).toInt > 14
      case "green" => nc(0).toInt > 13
      case "red"  => nc(0).toInt > 12
  })
 }
}
