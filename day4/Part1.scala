import scala.io.Source._
import scala.math._

object Part1 { 
  def main(Args: Array[String]) = {
    println(fromFile("input.txt").getLines.map(findScore).sum)
  }

  def findScore(line: String): Int = { 
    var cards: Array[Array[String]] = line.split("\\:")(1).split("\\|").map(l => l.trim.split(" +"))
    (floor(cards(1).filter(c => cards(0).contains(c)).map(c => {
      2}).product) / 2).toInt
  }
}
