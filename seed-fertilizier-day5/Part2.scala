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

object Part2 { 
  def main(Args: Array[String]) = {

    var seedRanges: Array[(Long, Long)] = Array()
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
    println(seedRanges.map(seedRange => find(Category.SEED, Category.SOIL, seedRange, maps)).min)
  }

  def find(source: Category, 
           destination: Category, 
           seedRange: (Long, Long), 
           maps: Array[FarmMap]
  ) : Long = {
    println("Finding " + source.toString + " => " + destination.toString)
    // assumption: never gonna be missing categories
    val fmap: FarmMap = maps.filter(farmMap => farmMap.from == source)(0)
    var infos: Array[FarmMapInfo] = fmap.info
    var rangeStart: Long = seedRange(0)
    var rangeEnd: Long = seedRange(0) + seedRange(1) 
    val rangeLength: Long = seedRange(1)
    var ranges: Array[(Long, Long)] = Array()
    infos.foreach(i => println("starting at " + i.sourceRangeStart))
    infos = infos.sortWith((i, o) => i.sourceRangeStart < o.sourceRangeStart)
    infos.foreach(info => {
      val sourceStart = info.sourceRangeStart
      val destStart = info.destinationRangeStart
      val sourceEnd = info.sourceRangeStart + info.rangeLength
      // starts outside (left)
      if (rangeStart < sourceStart) { 
        if (rangeEnd < sourceStart) { // ends outside (left)
          ranges = ranges :+ (rangeStart, rangeLength)
        }
        
        else if (rangeEnd < sourceEnd) { // ends inside
          var outsideLength = rangeLength - (sourceEnd - rangeEnd)
          var insideLength = rangeLength - outsideLength
          ranges = ranges :+ (rangeStart, rangeStart, outsideLength)
          ranges = ranges :+ (destStart, insideLength)
        }
        
        else { // ends outside (right)
          var outsideLength = rangeLength - (sour
        }
        
      }
      // starts inside
      else if (rangeStart < sourceEnd) {
        if (rangeEnd < sourceEnd) { // ends inside 
        
        }
        else { // ends outside
        }
      }
      // starts and ends outside (right)
      else {
        
      }

    })

    0
  }

  def isDigit(c: Char): Boolean = {
    c - '0' >= 0 && c - '0' < 10
  }
}
