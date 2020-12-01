import scala.annotation.tailrec
import scala.io.Source

object day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d1p1.txt").getLines().toList
    println(solveDay1p1(lines.map(_.toInt)))
    println(solveDay1p2(lines.map(_.toInt)))
    println(solveDay1p1ButMoreFun(lines.map(_.toInt)))
    println(solveDay1p2ButMoreFun(lines.map(_.toInt)))
  }

  //desugars to double-nested foreach, pretty poopy
  def solveDay1p1(input: List[Int]): Int =
    input.filter(x => input.contains(2020 - x)).map(x => (x, 2020 - x)).map(x => x._1 * x._2).head

  //desugars to triple-nested foreach, very poopy
  def solveDay1p2(input: List[Int]): Int =
    input.map(q => (q, input.find(w => input.contains(2020 - q - w)))).collect { case (x, Some(y)) => x * y * (2020 - x - y) }.head

  def solveDay1p1ButMoreFun(input: List[Int]): Int =
    solveForTarget(2020, input).get

  //run our previous nlogn solution on each of n inputs, so we're looking at n^2logn
  def solveDay1p2ButMoreFun(input: List[Int]): Int =
    input.flatMap(q => solveForTarget(2020 - q, input).map(_ * q)).head


  //sort the original input and then split it into two subsets based on whether each element is smaller than target/2
  //we know any solution that exists will use one element from each sublist
  //sort and then solve, so we're looking at O(nlogn) to sort and then n to solve, so nlogn
  def solveForTarget(target: Int, input: List[Int]): Option[Int] = {
    val partitionedInput = input.sorted.partition(_ < target / 2)
    lel(partitionedInput._1, partitionedInput._2.reverse, target).left.get.map(x => x._1 * x._2)
  }

  //this method bakes in a lot of assumptions about it's inputs and the types get wacky but it's an O(n) solution (on sorted inputs)
  // to the subset sum problem where we look at each element once
  @tailrec
  def lel(a: List[Int], b: List[Int], target: Int): Either[Option[(Int, Int)], List[Int]] = {
    if(a.isEmpty || b.isEmpty) Left(None)
    else if (a.head + b.head == target) Left(Some(a.head, b.head))
    else if (a.head + b.head > target) lel(a, b.tail, target)
    else lel(a.tail, b, target)
  }
}