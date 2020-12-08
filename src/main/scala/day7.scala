import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.tailrec
import scala.io.Source

object day7 {

  def main(args: Array[String]): Unit = {
    val extraInput = "shiny gold bag"

    //real hacky stuff up here
    //at some point i'll learn to make things easier on myself by taking 2 seconds to build a real data structure so I don't confuse myself later
    val part1 = Source.fromResource("d7.txt").getLines().filterNot(_.contains("no other bags")).map(_.replace("bags", "bag")).toList
    val mappedBags = part1.map(row => {
      val key = row.substring(0, row.indexOf(" contain "))
      val values = row.replace(".", "").substring(key.length + 9).split(", ").map(section => {
        val pattern(count, bag) = section
        (bag, count.toInt)
      }).toList
      (key, values)
      //so what this all ends up with is a flattened tree structure, where instead of having nodes with links between them i just have a single
      //map of all the edges. honestly not my smartest manuever
    }).toMap

    val search = bfs(mappedBags, Set("shiny gold bag"))
    println(search.size - 1)
    val e = bfs(mappedBags, extraInput)
    println(e - 1)
  }

  val pattern = """(\d+)\s(\w+\s\w+\sbags*.*)""".r

  //i may start implementing some of these recursive functions with more global state since I kind of hate how complex these
  //function definitions get. not really a bfs because it's actually insanely inefficient since it repeatedly iterates over
  //entire map object to find nodes for each step. definitely not my best work but it's late
  @tailrec
  def bfs(map: Map[String, List[(String, Int)]], seen: Set[String]): Set[String] = {
    val union = map.filter(tuple => tuple._2.exists(x => seen.contains(x._1))).keySet union seen
    if (union.size == seen.size) seen
    else bfs(map, union)
  }

  //sadly no tailrec here
  def bfs(map: Map[String, List[(String, Int)]], current: String): Long = {
    map.get(current) match {
        //only need the none match because I was lazy handling the 'no other bags' case
      case None => 1
      case Some(currentBag) =>
        //took me way to long to realize i needed to add one to account for the bag itself here
        currentBag.map(subBag => bfs(map, subBag._1) * subBag._2 ).sum + 1
    }
  }
}