import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.tailrec
import scala.io.Source

object day5 {

  def main(args: Array[String]): Unit = {
    //so this sort of stream processing should hypothetically be faster than even a raw file.readlines
    val stats = Files.lines(Paths.get("/Users/nate.fitzgerald/play/advent2020/src/main/resources/d5.txt"))
      .mapToLong(element => FindRow(element.take(7), 'B') * 8 + FindRow(element.drop(7), 'R'))
      //we need the min value for part one, and then both min and max for part 2,
      //rather than wire that up into a custom accumulator I just used their easy stuff
      .summaryStatistics()
    println(stats.getMax)
    //fastest way to find a missing element is to sum the elements and subtract it from what we know the total is
    val actualSum = (stats.getMin to stats.getMax).sum
    println(actualSum - stats.getSum)
  }

  def FindRow(element : String, uh: Char): Long = FindRow(element, 0, 0, uh)
  //have a hunch this might get multidimensional
  //oh ok it didnt but for the record I was psyched for this plane to go to n dimensions
  @tailrec
  def FindRow(element: String, ptr: Int, count: Long, upperHalf: Char): Long = {
    if(ptr == element.length) count
    else if(element(ptr) == upperHalf) FindRow(element, ptr + 1, count = count + Math.pow(2, element.length).toLong / Math.pow(2, ptr + 1).toInt, upperHalf)
    else FindRow(element, ptr + 1, count, upperHalf)
  }
}

