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
    val isIn: Boolean = seed >= sourceRangeStart && seed < sourceRangeStart + rangeLength
    println("seed " + seed + " in range ? " + this.toString + " " + isIn)
    isIn
  }

  def apply(seed: Long): Long = {
    println("seed " + seed + " ==> " + (destinationRangeStart + (seed - sourceRangeStart)))
    destinationRangeStart + (seed - sourceRangeStart)
  }

  override def toString: String = { 
    "[" + sourceRangeStart + ", " + (rangeLength + sourceRangeStart) + ")"
  }
}

object Part1 { 
  def main(Args: Array[String]) = {

    var seedRanges: Array[(Long, Long)] = Array()
    var maps: Array[FarmMap] = Array()
    var currentHeader: String = ""
    var currentMap: Array[FarmMapInfo] = Array()
    fromFile("input.txt").getLines.filter(l => !l.trim.isEmpty).foreach(line => 
    {

      if (isDigit(line.charAt(0))) {
        var info: Array[Long] = line.split(" ").map(s => s.toLong)
        currentMap = currentMap :+ FarmMapInfo(info(0), info(1), info(2))
      }
      else { 
        if (line contains "seeds") {
          println("seeds was matched")
          val seeds = line.split(":")(1).trim.split(" ").map(s => s.toLong)
          for (i <- 0 to seeds.size - 2) {
            seedRanges = seedRanges :+ (seeds(i), seeds(i+1))
          }
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
    maps.foreach(println)
    println(seeds.map(seed => find(Category.SEED, Category.SOIL, seed, maps)).min)
  }

  def find(source: Category, 
           destination: Category, 
           seed: Long, 
           maps: Array[FarmMap]
  ) : Long = {
    println("Finding " + source.toString + " => " + destination.toString)
    // assumption: never gonna be missing categories
    val x: Long = maps.filter(farmMap => farmMap.from == source)(0)
        .info.find(farmMapInfo => farmMapInfo.inRange(seed)) match
          case Some(info) => info.apply(seed)
          case None => seed
    
    if (destination == Category.LOCATION) {
      x
    }
    else find(
      destination, 
      maps.filter(farmMap => farmMap.from == destination)(0).to,
      x,
      maps)
  }

  def isDigit(c: Char): Boolean = {
    c - '0' >= 0 && c - '0' < 10
  }
}
