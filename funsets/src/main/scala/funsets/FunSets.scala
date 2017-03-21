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
  type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean =
    s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singleElementSet(elem: Int): FunSet = {
    def f(x: Int): Boolean = (x == elem)
    f
  }

  /*
   * OU
   * def singleElementSet(elem: Int): FunSet = (x => x == elem)
   */

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = {
    def f(x: Int): Boolean = (s(x) || t(x))
    f
  }
  /*
   * OU
   *def union(d: FunSet, a: FunSet): FunSet = (x => contains(d,x) || contains(a,x))
   */

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet =
    {
      def f(x: Int): Boolean = (s(x) && t(x))
      f
    }

  /*
   * OU
   * def intersect(v: FunSet, i: FunSet): FunSet = (x => contains(v,x) && contains(i,x))
   */

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet =
    {
      def f(x: Int): Boolean = (s(x) && !t(x))
      f
    }

  /*
   * OU
   * def diff(a: FunSet, z: FunSet): FunSet = (x => contains(a,x) && !contains(z,x))
   */

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet =
    {
      def f(x: Int): Boolean = (s(x) && p(x))
      f
    }

  /*
   * OU
   * def filter(f: FunSet, i: Int => Boolean): FunSet = (x => contains(f,x) && i(x))
   */

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
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
   * forall => Il n'existe pas d'entier 'bounded' tel que p(x) est faux
   * ! forall => Il existe un entier 'bounded' tel que p(x) est faux
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet =
    (y => exists(s, x => y == f(x)))

  def toSet(ints: List[Int]): FunSet =
    if (ints.isEmpty) x => false
    else union(singleElementSet(ints.head), toSet(ints.tail))

  /*def toList(set: FunSet): List[Int] = {

  }*/
  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet) {
    println(toString(s))
  }
}
