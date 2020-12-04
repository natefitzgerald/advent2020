import scala.annotation.tailrec
import scala.io.Source

object day3 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d3.txt").getLines().toList
    println(d3p1(lines))
    val givenSlopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    println(d3p2(lines, givenSlopes))
  }

  def d3p1(input: List[String]) =
    singleMove(0, 0, Grid(input.mkString(""), input.head.length, input.length, 3, 1))

  def d3p2(input: List[String], slopes: List[(Int, Int)]) =
    slopes.map(x => sickOfThisShit(input, x._1, x._2)).map(_.toLong).product

  def d3p2v2(input: List[String], slopes: List[(Int, Int)]) =
    slopes.map(x => singleMove(0, 0, Grid(input.mkString(""), input.head.length, input.length, x._1, x._2))).map(_.toLong).product

  //i got sick of trying to figure out why my recursive function wasnt working so I wrote this one too
  //turns out i was overflowing the int when i was mutiplying the total
  //fucking dan's input didn't overflow
  //turns out if you do it like this it's obviously extremely straightforward
  def sickOfThisShit(input: List[String], xdis: Int, ydis: Int) = {
    var x = 0
    var y = 0
    val width = input.head.length
    var count = 0
    do {
      if (input(y)(x) == '#') count = count + 1
      x = (x + xdis) % width
      y = y + ydis
    } while (y < input.length)
    count
  }

  //this is actually a really bad version of this
  //as i iterated later I realized you only actually need one case, so having two recursive calls is a little much
  @tailrec
  def singleMove(count: Int, pos: Int, grid: Grid): Int = {
    if(pos >= grid.input.length) count
    else if (pos % grid.width + grid.xdisplacement < grid.width) singleMove(count + grid.input(pos).equals('#').compare(false), pos + (grid.width * grid.ydisplacement) + grid.xdisplacement, grid)
    else singleMove(count + grid.input(pos).equals('#').compare(false), pos + grid.xdisplacement + ((grid.ydisplacement - 1) * grid.width), grid)
  }

  //this was a bad idea, it wastes stack space reallocating the same variables over and over again and it doesn't even save any space
  case class Grid(input: String, width: Int, length: Int, xdisplacement: Int, ydisplacement: Int)
}