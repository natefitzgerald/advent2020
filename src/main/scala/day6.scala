import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.tailrec
import scala.io.Source

object day6 {

  def main(args: Array[String]): Unit = {
    //apparently scala doesnt have an api that allows me to just get the entire file as a string and they seem to really prefer I access it as an iterator
    //so that's why im adding newlines
    val part1 = Source.fromResource("d6.txt").getLines().map(x => if(x == "") "\n" else x).mkString.split("\n").map(_.toSet).map(_.size).sum
    println(part1)

    //ignore that whole middle section, that's just finessing the input (done poorly)
    //the meat is the .fold at the end where I iterate through grabbing the intersection of the sets that are everyone's answers, using all possible characters as the seed value
    val part2 = Source.fromResource("d6.txt").getLines().map(x => if(x == "") "\n" else x).mkString(" ").split("\n").map(x => x.split(" ").toList.filter(_ != "").map(_.toSet).fold("qwertyuiopasdfghjklzxcvbnm".toSet)((a1:Set[Char], a2:Set[Char]) => a1.intersect(a2))).map(_.size).sum
    println(part2)
  }

}