import scala.io.Source._
import scala.math._

class Pair[T](val left: T, val right: T)

object Part2 { 

  def main(args: Array[String]) = {
    val instructions: String = fromFile(args(0)).getLines.find(_=>true).orElse(Some("")).get

    val nodes: Map[String, Pair[String]] = fromFile(args(0)).getLines
      .filter(l=> l.contains("="))
      .map(l => (l.split(" = ")(0) -> Pair(
        l.split(" = ")(1).split(",")(0).substring(1,4),
        l.split(" = ")(1).split(",")(1).substring(1,4))))
      .toMap
    val steps: Array[Int] = nodes.keys.filter(k=> k.endsWith("A")).map(k => findStepsTo(k, instructions, nodes)).toArray
    println(lcm(steps))
  }

  def findStepsTo(start: String, 
                  instructions: String, 
                  nodes: Map[String, Pair[String]]): Int = 
  {
    var current: String = start
    var steps: Int = 0
    while (!current.endsWith("Z")) {
      var sb: String = current + " -> " 
      val curr: Pair[String] = nodes.get(current) match 
        case Some(p) => p
        case None => return 0
      
      current = instructions.charAt(steps % instructions.length) match 
        case 'L' => curr.left
        case 'R' => curr.right
      steps += 1
    }
    steps
  }

  def lcm(numbers: Array[Int]): Long = { 
    val allPrimeFactors: Array[List[Int]] = numbers.map(factorize).toArray
    var maxPrimePowers: Map[Int, Int] = Map()

    for (i <- 0 until allPrimeFactors.size) { 
      val pf: List[Int] = allPrimeFactors(i)
      val pows: Map[Int, List[Int]] = pf.groupBy(identity)
      pows.foreach((p, q) => {
        if (q.size > maxPrimePowers.get(p).orElse(Some(0)).get) { 
          maxPrimePowers = maxPrimePowers - p + (p-> q.size)
        }
      })
    }
    maxPrimePowers.map((k,v) => pow(k, v).floor.toLong).product
  }

  def factorize(x: Int): List[Int] = {
  
    def foo(x: Int, a: Int): List[Int] = if (a * a > x) List(x) else
      x % a match {
        case 0 => a :: foo(x / a, a)
        case _ => foo(x, a + 1)
      }
    foo(x, 2)
  }
}
