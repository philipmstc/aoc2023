import scala.io.Source._
import scala.collection.mutable.Map

class Card() {
  var number: Int = 0
  var winning: Array[String] = Array()
  var youHave: Array[String] = Array()

  def this(line: String) = {
    this()
    val l1 = line.split(":")
    val number = l1(0).substring(l1(0).lastIndexOf(" ")+1)
    this.number = number.toInt
    val l2 = l1(1).split("\\|")
    val winning = l2(0).trim.split(" +")
    this.winning = winning
    val youHave = l2(1).trim.split(" +")
    this.youHave = youHave
  }
}

object Part2 {
  def main(Args: Array[String]) = {
    val cards: Array[Card] = fromFile("input.txt").getLines.map(l=> Card(l)).toArray
    var toProcess: Map[Card, Int] = Map[Card, Int]() ++= cards.map(card => (card, 1)).toMap
    var total = cards.size
    var i: Int = 0
    while (i < cards.size) {
      toProcess(cards(i)) = toProcess(cards(i)) - 1
      
      val card: Card = cards(i)
      val score: Int = card.youHave.filter(n=>card.winning.contains(n)).size
      for (x <- 1 to score) {
        total = total + 1
        toProcess(cards(i + x)) = toProcess(cards(i+x)) + 1  
      }
      if (toProcess(cards(i)) == 0) {
        i = i + 1 
      }
    }
    println(total)
  }
}
