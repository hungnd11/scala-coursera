package funsets

import scala.annotation.tailrec


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  // Because we characterise a set as a boolean function
  // A singleton set of element 'elem' is a function of parameter e such that
  // its value is true iff the parameter e equals to elem.
  def singletonSet(elem: Int): Set =
    (e: Int) => e == elem
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set =
    (e: Int) => s(e) || t(e)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set =
    (e: Int) => s(e) && t(e)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set =
    (e: Int) => s(e) && !t(e)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  // Filter out all elements belonging to s (s(e) == true)
  // which satisfy the predicate (p(e) == true)
  def filter(s: Set, p: Int => Boolean): Set =
    (e: Int) => s(e) && p(e)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  // This function checks whether all elements in range [-bound, bound]
  // which belong to s satisfy the predicate
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  // The condition is equivalent to whether NOT all bounded integers within 's'
  // DO NOT satisfy predicate p
  def exists(s: Set, p: Int => Boolean): Boolean =
    !forall(s, e => !p(e))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  // e is an element of the mapped set iff
  // there exists an element x in the original set s such that
  // f(x) equals to e
  def map(s: Set, f: Int => Int): Set =
    (e: Int) => exists(s, x => f(x) == e)

  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
