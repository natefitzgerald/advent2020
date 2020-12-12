import scala.io.Source
import day10.Node

import java.util
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


//part 1 is basically just conway's game of life
//ive done this like a million times a million different ways so I just went in for an easy approach with nothing fancy
//a simple trick i learned a while ago to avoid having to write 'guards' to avoid out-of-bounds errors you can surround
//the input with empty tiles (floor tiles in this case) that can't ever change
object day11 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d11.txt").getLines().map(_.toCharArray).toArray
    val done = hello(0, lines)
    println(done)
  }

  //this is to iterate through the arrays looking for the first duplicate, probably keep this for part 2
  @tailrec
  def hello(i: Int, arr: Array[Array[Char]]): Int = {
    val arrs = Seq(lel(arr), arr)
    val asums = arrs.map(h => h.map(x => x.map(y => if(y == '#') 1 else 0).sum).sum)
    if(asums.head == asums.tail.head) asums.head
    else hello(i + 1, arrs.head)
  }

  //it's like 5:30am and I dont want to do part 2 today, it'll probably require a full rewrite where I go to a list-based model
  //for storing the nodes and their adjacencies since I don't really care to finesse the logic below to account for part 2
  //it's kind of an interesting problem though so i look forward to attempting it at some point
  //it'd probably reward multiple indexes through, maybe I'll add state so we're not just fully recreating the array
  def lel(arr: Array[Array[Char]]): Array[Array[Char]] = {
    val newArr = new Array[Array[Char]](arr.length)
    newArr.indices.foreach(i => newArr(i) = new Array[Char](arr.head.length))
    arr.zipWithIndex.slice(1, arr.length - 1).foreach(y => {
      y._1.zipWithIndex.slice(1, arr.head.length - 1).foreach(x => {
        val n =
        //.compare(false) is a good way to turn a boolean into a 1 or 0 int value
          (arr(y._2 + 1)(x._2 - 1) == '#').compare(false) +
          (arr(y._2 + 1)(x._2) == '#').compare(false) +
          (arr(y._2 + 1)(x._2 + 1) == '#').compare(false) +
          (arr(y._2 - 1)(x._2 - 1) == '#').compare(false) +
          (arr(y._2 - 1)(x._2) == '#').compare(false) +
          (arr(y._2 - 1)(x._2 + 1) == '#').compare(false) +
          (arr(y._2)(x._2 + 1) == '#').compare(false) +
          (arr(y._2)(x._2 - 1) == '#').compare(false)
        if(arr(y._2)(x._2) == 'L' && n == 0)
          newArr(y._2)(x._2) = '#'
        else if(arr(y._2)(x._2) == '#' && n >= 4)
          newArr(y._2)(x._2) = 'L'
        else {
          newArr(y._2)(x._2) = arr(y._2)(x._2)
        }
      })
    })
    newArr
  }

  def printFerry(arr: Array[Array[Char]]): Unit = arr.foreach(c => println(new String(c)))
}

//2540 too high