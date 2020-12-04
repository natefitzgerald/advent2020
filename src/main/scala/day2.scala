import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object day2 {
  /*def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d2.txt").getLines().toList
    //decided to time my two versions
    //didnt bother finishing the recursive p2, it didn't seem that fun
    timeFuncs(lines,
      d2p1, d2p2, d2p1Fun
    )

    /**
      [info] Running day2
      6588355
      3034660
      2034592
     */
    //so the tail-recursive version is substantially faster even on smallish input sets
    //i thought about parallelizing it to be cool but since both functions can just partition the input set easily and reduce
    //it's not actually that interesting
  }*/

  //naiive solution, not even that slow all things considering
  //both parts split the input in the same way
  def shared(input: Seq[String]): Seq[(Int, Int, Char, String)] =
    input.map(q => q.split(" ")).map(q => (q(0).split("-")(0).toInt, q(0).split("-")(1).toInt, q(1)(0), q(2)))

  //part 1 needs to count the occurences and ensure it's between the two numbers
  def d2p1(input: Seq[String]): Int =
    shared(input).count(q => q._4.count(_ == q._3) >= q._1 && q._4.count(_ == q._3) <= q._2)

  //part 2 makes sure it's in one of the two positions, ^ is apparently scala for xor
  def d2p2(input: Seq[String]): Int =
    shared(input).count(q => q._4(q._1 - 1) == q._3 ^ q._4(q._2 - 1) == q._3)


  //this is where the fun was
  //tail-recurse through the input array, using a second tail-recursive function with a janky ass state machine to parse the input
  @tailrec
  def d2p1Fun(input: Seq[String], current: Int): Int = {
    input match {
      case Nil => current
      case head :: tail =>
        d2p1Fun(tail, current + lelarino(0, 0, 0, 0, head, -1))
    }
  }
  def d2p1Fun(input: Seq[String]): Int = d2p1Fun(input, 0)

  //this is said janky ass state machine
  //honestly this is kind of just a wacky for loop because in magic scala functional land it's somehow functional to
  //do pass short-lived stack variables between stack frames but not to do the same thing with mutable variables within function scope
  @tailrec
  def lelarino(state: Int, marker: Int, lower: Int, upper: Int, input: String, iMinus1: Int): Int = {
    val i = iMinus1 + 1
    if (state == 0 && input(i) == '-')
      lelarino(1, i, input.substring(marker, i).toInt, 0, input, i)
    else if (state == 1 && input(i) == ' ')
      lelarino(2, i, lower, input.substring(marker + 1, i).toInt, input, i)
    else if (state == 2 && input(i) == ':') {
      val count = doublerino(0, i, input(i - 1), input)
      if (count >= lower && count <= upper) 1
      else 0
    }
    else lelarino(state, marker, lower, upper, input, i)
  }

  //obviously we needed a third tail-recursive function to count the occurrences of the relevant character
  //there are a lot of optimizations one could make if you could run analytics on the input set, for example this would
  //perform poorly if the password was a billion characters long but became invalid after the second since there's no short-circuit logic
  @tailrec
  def doublerino(count: Int, j: Int, letter: Char, input: String): Int = {
    if (j == input.length) count
    else if (input(j) == letter) doublerino(count + 1, j + 1, letter, input)
    else doublerino(count, j + 1, letter, input)
  }

  def timeFuncs(input: Seq[String], inputFunctions: (Seq[String] => Int)*): Unit = {
    for(func <- inputFunctions) {
      val timer = System.nanoTime()
      func(input)
      println(System.nanoTime() - timer)
    }
  }

}