import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import java.nio.file.{Files, Paths}
import java.util.stream.{IntStream, Stream}
import scala.util.matching.Regex

object day4 {

  //this one wasn't my best work but frankly the problem was kind of tedious
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d4.txt").getLines()
    val lelerator = new Lelerator(lines)
    val count = lelerator.count(fullEntry => RuleMap.keys.forall(fullEntry.contains(_)))
    println(count)


    val lines2 = Source.fromResource("d4.txt").getLines()
    val lelerator2 = new Lelerator(lines2)
    val part2 = lelerator2.filter(fullEntry => RuleMap.keys.forall(fullEntry.contains(_)))
    val part2Count = part2.count(completeLine => {
      val s = split(completeLine)
      RuleMap.forall(regex => {
        s.get(regex._1).exists(x => {
          regex._2.findFirstIn(x).nonEmpty
        })
      })
    })
    println(part2Count)
  }

  //this was kind of fun, I'll probably do this again
  //kind of layer a second iterator over the naiive one that you get from the file where you can glob lines together
  //i was goofing off with more performant streams but there's no easy way to situationally merge lines without using
  // state and a closure or something
  class Lelerator(input: Iterator[String]) extends Iterator[String] {

    override def hasNext: Boolean = input.hasNext

    override def next(): String =
      findNext(input, List.empty).mkString(" ")

    //obviously im not done using tailrec this shit is sweet
    @tailrec
    private def findNext(input: Iterator[String], buffer: List[String]): List[String] = {
      if(!input.hasNext) return buffer
      val x = input.next()
      if (x.isEmpty) buffer
      else findNext(input, x :: buffer)
    }
  }

  //this wasn't my best work ever
  //when I wrote this component I hadn't realized I should be using regexes yet I was just concerned with splitting up the input
  def split(inputLine: String): Map[String, String] = {
    val padded = inputLine + " "
    RuleMap.keys.map(field => {
      val start = padded.indexOf(field) + field.length + 1
      (field, padded.substring(start, padded.indexOf(" ", start)))
    }).toMap
  }

 // val fields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  //ahaha can't forget those anchors fam, that was a good half hour down the drain
  val RuleMap: Map[String, Regex] =
    Map(
      "byr" -> "^(19[23456789][0-9]|200[012])$".r,
      "iyr" -> "^201[0-9]|2020$".r,
      "eyr" -> "^202[0-9]|2030$".r,
      "hgt" -> "^(1[5678][0-9]cm|19[0123]cm)|(59in|6[0-9]in|7[0123456]in)$".r,
      "hcl" -> "^#([0-9]|[a-f]){6}$".r,
      "ecl" -> "^amb|blu|brn|gry|grn|hzl|oth$".r,
      "pid" -> "^[0-9]{9}$".r
    )

}
/*
keeping this stuff for some other challenge
val stream: Stream[String] = Files.lines(Paths.get("/Users/nate.fitzgerald/play/advent2020/src/main/resources/d4.txt"))
val noNewlines: Stream[String] = stream.map(x => x.replace("\n", ""))
val condensedStream = noNewlines.filter(x => x.isEmpty)
val reduced: IntStream = condensedStream.mapToInt(row => if(fields.forall(row.contains(_))) 1 else 0 )
val total = reduced.sum()
println("total: " + total)
*/