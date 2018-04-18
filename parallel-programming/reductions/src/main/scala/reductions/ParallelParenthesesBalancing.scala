package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   *  I have a tail-recursive implementation at https://github.com/hungnd11/scala-coursera/blob/master/progfun1/recfun/src/main/scala/recfun/Main.scala
   */
  def balance(chars: Array[Char]): Boolean = {
    if (chars.length > 1) {
      var countPairs = 0
      var index = 0
      var encounteredLeft = false
      var encounteredRight = false
      var firstLeftParenthesis = -1
      var firstRightParenthesis = -1

      while (index < chars.length) {
        if (chars(index) == '(') {
          countPairs = countPairs + 1
          if (!encounteredLeft) {
            encounteredLeft = true
            firstLeftParenthesis = index
          }
        }
        if (chars(index) == ')') {
          countPairs = countPairs - 1
          if (!encounteredRight) {
            encounteredRight = true
            firstRightParenthesis = index
          }
        }
        index = index + 1
      }

      countPairs == 0 && (firstLeftParenthesis < firstRightParenthesis) && encounteredLeft && encounteredRight
    } else if (chars.length == 1 && (chars(0) == '(' || chars(0) == ')')) 
      false
    else 
      true
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    // the key to parallel balance() is how to count in parallel 
    def traverse(idx: Int, until: Int): Int = {
      if (until - idx <= threshold) {
        var i = idx
        var countPairs = 0
        
        while (i < until) {
          if (chars(i) == '(') countPairs = countPairs + 1
          if (chars(i) == ')') {
            countPairs = countPairs - 1
            // handle some examples: "))))(((("
            if (countPairs < 0 && idx == 0)
              countPairs = 0
          }
          i = i + 1
        }
        
        countPairs
      } else {
        val mid = idx + (until - idx) / 2
        val (l, r) = parallel(traverse(idx, mid), traverse(mid, until))
        l + r
      }
    }

    def reduce(from: Int, until: Int): Int = {
      traverse(from, until)
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
