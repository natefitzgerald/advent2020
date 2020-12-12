import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.io.Source

object day9 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d9.txt").getLines().map(_.toLong)
    val p1 = part1(lines, lines.take(25).toList, 0)
    val p2 = part2(Source.fromResource("d9.txt").getLines().map(_.toLong), p1, List.empty)
    println(p1, p2.min + p2.max)
  }

  //using an iterator in these tailrec methods sure is easy
  @tailrec
  def part1(input: Iterator[Long], previous: List[Long], index: Long): Long = {
    val current = input.next()
    if (!previous.exists(x => previous.contains(current - x))) return current
    part1(input, previous.tail ++ List(current), index + 1)
  }

  @tailrec
  def part2(input: Iterator[Long], goal: Long, prevs: List[Long]): List[Long] = {
    val current = input.next()
    checkForSubsum(goal, prevs, current) match {
      case Some(list) => list
      case None => part2(input, goal, prevs ++ List(current))
    }
  }

  //this could be made considerably more efficient easily
  @tailrec
  def checkForSubsum(goal: Long, prevs: List[Long], current: Long): Option[List[Long]] = {
    if (prevs.sum + current == goal) Some(prevs ++ List(current))
    else if (prevs.isEmpty) None
    else checkForSubsum(goal, prevs.tail, current)
  }
}



//i considered doing this one big boi, you could pretty easily wire this up to split the input so that it duplicates the
//seed so you can thread it easily and i bet that would work but im actually just gonna call this one an easy day and go
//play dota