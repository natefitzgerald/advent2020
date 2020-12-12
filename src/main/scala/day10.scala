import java.nio.file.{Files, Paths}
import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.io.Source

object day10 {

  //part 1 of today seemed super easy so I went way overboard and wrote basically my own arraylist
  //it uses lists of integers as a backing store and honestly im pretty that I actually got it working
  //hypothetically it'd probably actually be pretty fast since i took off all the guardrails so it's
  //only applicable to very specialized situations. the deep copy functionality doesn't really work

  //part 2 haunts my dreams and i refuse to look online for the solution even though I think im probably
  //missing some simple closed-form solution

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("d10.txt").getLines().map(_.toInt)
    val x = lines.foldLeft(new Node)((acc, v) => {
      acc.flip(v)
      acc
    })

    val c = check(x, 0, (0, 0))
    println(c._1 * c._2)
    println()
    println(check2(x, 0, 1L))
  }

  @tailrec
  def check(n: Node, ptr: Int, jumps: (Int, Int)): (Int, Int) = {
    if(ptr > n.deep) jumps
    else if(n.check(ptr + 1)) check(n, ptr + 1, (jumps._1 + 1, jumps._2))
    else if(n.check(ptr + 2))  check(n, ptr + 2, jumps)
    else check(n, ptr + 3, (jumps._1, jumps._2 + 1))
  }

  //some version of this probably works for part 2
  @tailrec
  def check2(n : Node, ptr: Int, acc: Long): Long = {
    if (ptr == n.deep) acc
    else if(n.check(ptr))
      check2(n, ptr + 1, acc * 1 )
    else 1
  }


  //so this behaves like an array of Booleans, you can check or flip a bit at an index
  //behind the scenes it's manually flipping bits in a component int or recursing down to the next lazily created node
  //in the list.
  class Node() {
    private var value: Int = 0
    private var next: Option[Node] = None
    var deep = 0

    def check(index: Int): Boolean = check2(index, this)

    //turns out you can't tailrec on a publicly extendable class method, so you need a nonrecursive public interface
    @tailrec
    private final def check2(index: Int, n: Node): Boolean = {
      if (index < 32) (n.value & (1 << index)) != 0
      else {
        n.next match {
          case Some(nextNode) => check2(index - 32, nextNode)
          case None => false
        }
      }
    }
    //flip a single bit either on or off
    def flip(index: Int): Unit = deep = Math.max(flip2(index, this, 0) * 32 + index % 32, deep)

    @tailrec
    private final def flip2(index: Int, n: Node, depth: Int): Int = {
      if (index < 32) {
        //if we're on the right int, xor the value with a 1 shifted left x times
        n.value = n.value ^ (1 << index)
        depth
      }
      else {
        //otherwise keep going down the chain till you're at the right int
        n.next match {
          case Some(nextNode) => flip2(index - 32, nextNode, depth + 1)
          case None => {
            n.next = Some(new Node)
            flip2(index - 32, n.next.get, depth + 1)
          }
        }
      }
    }

    //this is the deep copy stuff that's only half done, I wrote this originally to use for day 11's conway game of life
    //since it's actually pretty applicable there too. if i was serious about using this for that i'd probably write
    //a 2d (or n-dimensional) accessor but i got bored and moved on to real stuff since I couldn't get the deep copy to
    //work well and then realized that doing a deep copy at all would make it less efficient than a straight bool array
    def GetNodes(): List[Int] = getnodes(this, List(value))
    @tailrec
    private def getnodes(n: Node, l: List[Int]): List[Int] = {
      n.next match {
        case Some(next) => getnodes(next, l ++ List(this.value))
        case None => l ++ List(n.value)
      }
    }
    def FromNodes(l: List[Int]): Node = {
      val newNode = new Node()
      fromnodes(newNode, l)
    }
    private def fromnodes(n: Node, l: List[Int]): Node = {
      l match {
        case head :: Nil =>
          val newNode = new Node()
          n.value = head
          n.next = Some(newNode)
          newNode

        case head :: tail => {
          val newNode = new Node()
          n.value = head
          n.next = Some(newNode)
          fromnodes(newNode, tail)
        }
      }
    }
  }
}