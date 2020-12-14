import scala.io.Source
import day10.Node

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


//part 1 is basically just conway's game of life
//ive done this like a million times a million different ways so I just went in for an easy approach with nothing fancy
//a simple trick i learned a while ago to avoid having to write 'guards' to avoid out-of-bounds errors you can surround
//the input with empty tiles (floor tiles in this case) that can't ever change
//i used ! for a guard in part 2
object day11 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d11.txt").getLines().map(_.toCharArray).toArray
    val done = runToCompletion(0, lines, p1CheckAllSeats)
    println(done)

    val p2 = runToCompletion(0, lines, p2CheckAllSeats)
    println(p2)
  }

  @tailrec
  def runToCompletion(i: Int, arr: Array[Array[Char]], f: Array[Array[Char]] => Array[Array[Char]]): Int = {
    val arrs = Seq(f(arr), arr)
    val asums = arrs.map(h => h.map(x => x.map(y => if(y == '#') 1 else 0).sum).sum)
    if(asums.head == asums.tail.head) asums.head
    else runToCompletion(i + 1, arrs.head, f)
  }

  //it's like 5:30am and I dont want to do part 2 today, it'll probably require a full rewrite where I go to a list-based model
  //for storing the nodes and their adjacencies since I don't really care to finesse the logic below to account for part 2
  //it's kind of an interesting problem though so i look forward to attempting it at some point
  //it'd probably reward multiple indexes through, maybe I'll add state so we're not just fully recreating the array
  //can't be throwing away this much memory if i wanna run with the big boys(tm)
  def p1CheckAllSeats(arr: Array[Array[Char]]): Array[Array[Char]] = {
    val newArr = new Array[Array[Char]](arr.length)
    newArr.indices.foreach(i => newArr(i) = new Array[Char](arr.head.length))
    arr.zipWithIndex.slice(1, arr.length - 1).foreach(y => {
      y._1.zipWithIndex.slice(1, arr.head.length - 1).foreach(x => {
        val n = mutators.map(f => if(arr(f(x._2, y._2)._2)(f(x._2, y._2)._1) == '#') 1 else 0 ).sum
        if(arr(y._2)(x._2) == 'L' && n == 0) newArr(y._2)(x._2) = '#'
        else if(arr(y._2)(x._2) == '#' && n >= 4) newArr(y._2)(x._2) = 'L'
        else newArr(y._2)(x._2) = arr(y._2)(x._2)
      })
    })
    newArr
  }

  //using this code to 'mutate' the coordinates was an innovation from part 2 I went and applied to clean up part 1 a bunch
  val mutators: List[(Int, Int) => (Int, Int)] = List(
    (x, y) => (x - 1, y - 1),(x, y) => (x - 1, y),(x, y) => (x - 1, y + 1), (x, y) => (x + 1, y - 1),
    (x, y) => (x + 1, y), (x, y) => (x + 1, y + 1), (x, y) => (x, y - 1), (x, y) => (x, y + 1),
  )

  //basically the same as before
  def p2CheckAllSeats(arr: Array[Array[Char]]): Array[Array[Char]] = {
    val newArr = new Array[Array[Char]](arr.length)
    newArr.indices.foreach(i => newArr(i) = new Array[Char](arr.head.length))
    arr.zipWithIndex.slice(1, arr.length - 1).foreach(y => {
      y._1.zipWithIndex.slice(1, arr.head.length - 1).foreach(x => {
        val n = p2checkAllFromSeat(arr, x._2, y._2)
        if (n >= 5 && arr(y._2)(x._2) == '#') newArr(y._2)(x._2) = 'L'
        else if (arr(y._2)(x._2) == 'L' && n == 0) newArr(y._2)(x._2) = '#'
        else newArr(y._2)(x._2) = arr(y._2)(x._2)
      })
    })
    newArr
  }

  //use the list of mutators applied recursively to find the seats visible from the starting seat
  def p2checkAllFromSeat(arr: Array[Array[Char]], x: Int, y: Int): Int =
    mutators.map(p2CheckDirectionFromSeat(_)(arr, x, y)).count(_ == true)

  //going along checking seats in sequence, using the supplied function to iterate through nodes, eventually ending off the board
  //or in one of the terminal states
  @tailrec
  def p2CheckDirectionFromSeat(mutator: (Int, Int) => (Int, Int))(arr: Array[Array[Char]], x: Int, y: Int): Boolean = {
    val (xval, yval) = mutator(x, y)
    arr(yval)(xval) match {
      case '#' => true
      case '.' => p2CheckDirectionFromSeat(mutator)(arr, xval, yval)
      case otherChar => false
    }
  }
  def printFerry(arr: Array[Array[Char]]): Unit = arr.foreach(c => println(new String(c)))
}