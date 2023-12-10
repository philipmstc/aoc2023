import scala.io.Source._

class Pair[T](val left: T, val right: T)

object Part1 { 

  def main(args: Array[String]) = {
    val instructions: String = fromFile(args(0)).getLines.find(_=>true).orElse(Some("")).get

    val nodes: Map[String, Pair[String]] = fromFile(args(0)).getLines
      .filter(l=> l.contains("="))
      .map(l => (l.split(" = ")(0) -> Pair(
        l.split(" = ")(1).split(",")(0).substring(1,4),
        l.split(" = ")(1).split(",")(1).substring(1,4))))
      .toMap
    println(findStepsTo("ZZZ", instructions, nodes))
  }

  def findStepsTo(target: String, 
                  instructions: String, 
                  nodes: Map[String, Pair[String]]): Int = 
  {
    var current: String = "AAA"
    var steps: Int = 0
    while (current != target) {
      var sb: String = current + " -> " 
      val curr: Pair[String] = nodes.get(current) match 
        case Some(p) => p
        case None => return 0
      
      current = instructions.charAt(steps % instructions.length) match 
        case 'L' => curr.left
        case 'R' => curr.right
      //println(sb + current)
      steps += 1
    }
    steps
  }
}
