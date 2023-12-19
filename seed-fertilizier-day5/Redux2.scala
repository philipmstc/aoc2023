import scala.io.Source._

enum Category:
  case SEED, SOIL, FERTILIZER, WATER, LIGHT, TEMPERATURE, HUMIDITY, LOCATION

class FarmMap(val from: Category, val to: Category, val info: Array[FarmMapInfo]) {
  override def toString: String = { 
    from.toString + " -> " + to.toString + " {" + info.size + "}"
  }
}

class FarmMapInfo(val destinationRangeStart: Long,
                  val sourceRangeStart: Long,
                  val rangeLength: Long) {
  
  def inRange(seed: Long): Boolean = {   
    seed >= sourceRangeStart && seed < sourceRangeStart + rangeLength
  }

  def apply(seed: Long): Long = {
    destinationRangeStart + (seed - sourceRangeStart)
  }

  def unapply(seed: Long): Long = { 
    println(seed + " ==> "  + sourceRangeStart)
    sourceRangeStart
  }

  override def toString: String = { 
    "[" + sourceRangeStart + ", " + (rangeLength + sourceRangeStart) + ")"
  }
}

object Redux2 { 
  def main(Args: Array[String]) = {

    var seeds: Array[Long] = Array()
    var maps: Array[FarmMap] = Array()
    var currentHeader: String = ""
    var currentMap: Array[FarmMapInfo] = Array()
    fromFile("test.txt").getLines.filter(l => !l.trim.isEmpty).foreach(line => 
    {

      if (isDigit(line.charAt(0))) {
        var info: Array[Long] = line.split(" ").map(s => s.toLong)
        currentMap = currentMap :+ FarmMapInfo(info(0), info(1), info(2))
      }
      else { 
        if (line contains "seeds") {
          seeds = line.split(":")(1).trim.split(" ").map(s => s.toLong)
        }
        else { 
          if (currentHeader != "") {
            maps = maps :+ FarmMap(
              Category.valueOf(currentHeader.split("-to-")(0).toUpperCase),
              Category.valueOf(currentHeader.split("-to-")(1).toUpperCase),
              currentMap
            )
          }
          currentMap = Array()
          currentHeader = line.split(" ")(0) // xxxx-to-yyyy
        }

      }
    })
    maps = maps :+ FarmMap(
      Category.valueOf(currentHeader.split("-to-")(0).toUpperCase),
      Category.valueOf(currentHeader.split("-to-")(1).toUpperCase),
      currentMap
    )
    var seed: Long = currentMap.map(mapInfo => mapInfo.destinationRangeStart).min 
    println(find(Category.LOCATION, Category.HUMIDITY, seed, maps))
  }

  def find(source: Category, 
           destination: Category, 
           seed: Long, 
           maps: Array[FarmMap]
  ) : Long = {
    val transformed: Long = maps.filter(farmMap => farmMap.to == source)(0)
        .info.find(farmMapInfo => farmMapInfo.inRange(seed)) match
          case Some(info) => info.unapply(seed)
          case None => seed
    
    destination match
      case Category.SEED => transformed
      case _ => find(
        destination, 
        maps.filter(farmMap => farmMap.to == destination)(0).from,
        transformed,
        maps)
  }

  def isDigit(c: Char): Boolean = {
    c - '0' >= 0 && c - '0' < 10
  }
}
