package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val m = if (a < b) a else b
    findMin(h) == m
  }

  property("delMin") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("delMin2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val m = if (a > b) a else b
    findMin(deleteMin(h)) == m
  }

  property("delMin3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    val m = if (a > b) if (a > c) a else c else if (c > b) c else b
    findMin(deleteMin(deleteMin(h))) == m
  }

  property("sorted") = forAll { (h: H) =>
    @scala.annotation.tailrec
    def isSorted(prevMin: Int, currHeap: H): Boolean = {
      if(isEmpty(currHeap))
        true
      else if (prevMin > findMin(currHeap))
        false
      else
        isSorted(findMin(currHeap), deleteMin(currHeap))
    }

    isSorted(scala.Int.MinValue, h)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val meldedMin = findMin(meld(h1, h2))
    meldedMin == scala.math.min(findMin(h1), findMin(h2))
  }
}
