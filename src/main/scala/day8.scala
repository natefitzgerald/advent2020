import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object day8 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d8.txt").getLines().toList
    val executed = execute(lines, 0, 0, ArrayBuffer.fill(lines.size)(false))
    println(executed)

    val p2 = new Permuter(lines).zipWithIndex.map(x =>  (x._2, execute(x._1, 0, 0, ArrayBuffer.fill(lines.size)(false)))).filter(x => x._2._2).toList
    println(p2)
  }

  @tailrec
  def execute(input: List[String], ptr: Int, acc: Long, executed: ArrayBuffer[Boolean]): (Long, Boolean) = {
    //true if we concluded by executing the last instruction
    if(ptr == input.size - 1) (acc, true)
    //false if we end with a loop
    else if (executed(ptr)) (acc, false)
    else {
      executed(ptr) = true
      input(ptr).substring(0, 3) match {
        case "nop" => execute(input, ptr + 1, acc, executed)
        case "jmp" => execute(input, ptr + input(ptr).substring(4).toInt, acc, executed)
        case _ => execute(input, ptr + 1, acc + input(ptr).substring(4).toInt, executed)
      }
    }
  }

  //i kinda think an efficient solution here would involve backtracking from the end since that seems pretty easily possible
  //but since I didn't want to completely rewrite this I went with a repeated recycling of my original approach while mutating
  //the
  class Permuter(input: List[String]) extends Iterator[List[String]] {
    //setting this at zero means we don't mutate the first element ever but i think that's probably ok
    var ptr = 0
    override def hasNext: Boolean = ptr != input.size - 1
    override def next(): List[String] = {
      ptr = ptr + 1
      input(ptr - 1).substring(0, 3) match {
        case "nop" =>
          //reconstruct the list with the modified instruction
          input.take(ptr - 1) ++ List(input(ptr - 1).replace("nop", "jmp")) ++ input.drop(ptr)
        case "jmp" =>
          input.take(ptr - 1) ++ List(input(ptr - 1).replace("jmp", "nop")) ++ input.drop(ptr)
        case _ =>
         // i'd really like to be able to use this.next() here to skip input that we're not going to mutate but it turns out that
          //the last instruction is an acc so doing that produces an error and if I wanted to avoid it i'd have to add logic to .hasNext
          input
      }
    }
  }
}