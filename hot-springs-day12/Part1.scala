import scala.io.Source._
import scala.math._

object Part1 {

  def main(args: Array[String]) = {
    println(fromFile(args(0)).getLines.filter(l => !l.startsWith("--")).map(numberOfPossibilities).sum)
  }

  def numberOfPossibilities(conditionRecord: String): Int = {
    val Array(conditions, groups) = conditionRecord.split(" ")
    val groupSizes: Array[Int] = groups.split(",").map(s=>s.toInt).toArray
    val possibilities: Int = calculatePossibilities(groupSizes, 1, "<" + conditions + ">", groupSizes).size
    possibilities
  }

  def calculatePossibilities(original: Array[Int], startIndex: Int, conditions: String, groupSizes: Array[Int]): Set[String] = {
    var solns: Set[String] = Set()

    if (groupSizes.size == 0) { 
      return (conditions.substring(startIndex).contains("#")) match 
        case true => Set()
        case false => { Set(conditions.substring(1,conditions.size-1)) }
    }

    val group = groupSizes(0)
    for (i <- startIndex until conditions.size-group) {
      val left = i-1
      val right = i + group + 1
      if (conditions(left) == '#') {
      }
      else if (conditions(right-1) == '#') {
      }
      else if (conditions.substring(left+1, right-1).matches("^[#?]+$")) {
        val newcond = conditions.substring(0, left+1) + ("#"*group)+ conditions.substring(right-1)
        solns = solns ++ calculatePossibilities(original, right, newcond, groupSizes.tail)
      }
    } 
    solns.filter(soln => isValid(soln, original)).toSet
  }

  def isValid(soln: String, groupSizes: Array[Int]): Boolean = {
    var currentGroup = 0
    var groupIndex = 0
    var inGroup = false

    for (i <- 0 until soln.size) {
      if (!inGroup) {
        if (soln(i) == '#') {
          if (groupIndex >= groupSizes.size || currentGroup > 0) {
            return false
          }
          currentGroup = groupSizes(groupIndex) - 1
          groupIndex += 1
          inGroup = true
        }
      }
      else {
        if (soln(i) == '#') { 
          currentGroup -= 1
          if (currentGroup == 0) {
            inGroup = false
          }
        }
        else {
          inGroup = false
        }
      }
    }
    true
  }
}
