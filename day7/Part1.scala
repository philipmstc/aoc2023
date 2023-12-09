import scala.io.Source._

class Hand(val cards: String, val bid: Int) {
  def <(other: Hand): Boolean = {
    if (this.buckets() != other.buckets()) {
      this.buckets() < other.buckets()
    }
    else { 
      this.tokens() < other.tokens() 
    }
  }

  def tokens(): String = {
    this.cards
      .replaceAll("T", "a")
      .replaceAll("J", "b")
      .replaceAll("Q", "c")
      .replaceAll("K", "d")
      .replaceAll("A", "e")
  }

  def buckets(): Int = { 
    cards
      .groupBy(identity)
      .mapValues(_.map(_ => 1).reduce(_+_))
      .values
      .toArray
      .sortWith(_ > _)
      .map(i => i.toString)
      .reduce(_+_)
      .padTo(5, '0')
      .toInt
  }

}
object Part1 {
  
  def main(args: Array[String]) = {
    val hands: Array[Hand] = fromFile(args(0)).getLines.map(toHand).toArray.sortWith(_ < _)
    var total: Int = 0
    for (i <- 0 until hands.size) {
      val hand: Hand = hands(i)
      total += (i+1)*hand.bid
    }
    println(total)
  }

  def toHand(line: String): Hand = { 
    val splits = line.split(" ")
    Hand(splits(0), splits(1).toInt)
  }
}
