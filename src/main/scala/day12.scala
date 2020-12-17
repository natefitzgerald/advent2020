import scala.io.Source
import day10.Node

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

//part 1 works great
//part 2 works on the example input but something's wrong on the larger input set and I'd rather play dota than fix it
object day12 {
  def main(args: Array[String]): Unit = {
   val p1 = part1(Source.fromResource("d12.txt").getLines())
   println(p1, Math.abs(p1.x) + Math.abs(p1.y))

    val p2 = part2(Source.fromResource("d12.txt").getLines(),Location(10,1,1), Location(0,0,1))
    println(p2, Math.abs(p2.x) + Math.abs(p2.y))
  }


  //used a map for part 1 but for whatever reason felt like using recursion for part 2
  @tailrec
  def part2(lines: Iterator[String], w: Location, s: Location): Location = {
    if(!lines.hasNext) s
    else {
      val str = lines.next()
      str.charAt(0) match {
        case 'N' => part2(lines, Location(w.x, w.y + str.drop(1).toInt, w.d), s)
        case 'E' => part2(lines, Location(w.x + str.drop(1).toInt, w.y, w.d), s)
        case 'S' => part2(lines, Location(w.x, w.y - str.drop(1).toInt, w.d), s)
        case 'W' => part2(lines, Location(w.x - str.drop(1).toInt, w.y, w.d), s)
        case 'R' => part2(lines, rotateCC(w, s, str.drop(1).toInt / 90), s)
        case 'L' => part2(lines, rotateCC(w, s, str.drop(1).toInt / 90 + 2), s)
        case 'F' =>
          //yea this is goofy get over it
          val (newWp, newS) = Range(0, str.drop(1).toInt).foldLeft((w, s)) { (ls, i) =>
            (Location(ls._1.x + w.x - s.x, ls._1.y + w.y - s.y, ls._1.d), Location(ls._2.x + w.x - s.x, ls._2.y + w.y - s.y, ls._2.d))
          }
          part2(lines, newWp, newS)
      }
    }
  }

  //just rotate counterclockwise 90 degrees each time and do it multiple times if needed
  //i have a hunch that this is the issue and I need to actually be rotating the other direction
  //originally this method was doing sin/cos math before i realized i just a huge moron
  def rotateCC(wp: Location, ship: Location, times: Int): Location = {
    def subtract(l1: Location, l2: Location): Location = Location(l1.x - l2.x, l1.y - l2.y, l1.d)
    if(times == 0) wp
    else if (times > 3) rotateCC(wp, ship, times - 4)
    else {
      val origin = subtract(ship, wp)
      rotateCC(Location(-origin.y + ship.x, origin.x + ship.y, ship.d), ship, times - 1)
    }
  }

  def part1(lines: Iterator[String]): Location = {
    lines.foldLeft(Location(0L, 0L, 1L)) {
      (l, s) => {
        s.charAt(0) match {
          case 'N' => Location(l.x, l.y + s.drop(1).toInt, l.d)
          case 'E' => Location(l.x + s.drop(1).toInt, l.y, l.d)
          case 'S' => Location(l.x, l.y - s.drop(1).toInt, l.d)
          case 'W' => Location(l.x - s.drop(1).toInt, l.y, l.d)
          case 'R' => Location(l.x, l.y, (l.d + s.drop(1).toInt / 90) % 4)
          case 'L' => Location(l.x, l.y, (l.d - s.drop(1).toInt / 90 + 4) % 4)
          case 'F' => directions(l.d)(l, s.drop(1).toInt)
        }
      }
    }
  }
  case class Location(x: Long, y: Long, d: Long)

  val directions: Map[Long, (Location, Long) => Location] = Map(
    0L -> { (l, d) => Location(l.x, l.y + d, l.d) },
    1L -> { (l, d) => Location(l.x + d, l.y, l.d) },
    2L -> { (l, d) => Location(l.x, l.y - d, l.d) },
    3L -> { (l, d) => Location(l.x - d, l.y, l.d) }
  )
}
//48000 too high