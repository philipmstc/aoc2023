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
      .replaceAll("J", "0")
      .replaceAll("Q", "c")
      .replaceAll("K", "d")
      .replaceAll("A", "e")
  }

  def buckets(): Int = this.buckets(false)

  def buckets(debug: Boolean): Int = { 
    val buckets: Map[Char, Int] = cards
      .groupBy(identity)
      .mapValues(_.map(_ => 1).reduce(_+_))
      .toMap

    if (cards == "JJJJJ") { 
      return 50000
    }

    val maxKey: Char = buckets.find{case (k,v) => k!='J' && v == buckets.filter{(q,p)=>q!='J'}.values.max} match
      case Some((k, v)) => k
      case None => 'J'
  
    val jokers: Int = buckets.get('J') match 
      case Some(x) => x 
      case None => 0
    
    val newMax: Int = buckets.get(maxKey) match 
      case Some(x) => x
      case None => 0

    val b2: Map[Char, Int] = buckets - 'J' + ('J' -> 0) - maxKey + (maxKey -> (newMax + jokers))
    b2.values
      .toArray
      .sortWith(_ > _)
      .map(i => i.toString)
      .reduce(_+_)
      .padTo(5, '0')
      .substring(0,5)
      .toInt
  }

}
object Part2 {
  
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
