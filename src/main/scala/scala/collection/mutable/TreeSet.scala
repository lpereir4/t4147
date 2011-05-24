/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

/** $factoryInfo
 *  @define Coll immutable.TreeSet
 *  @define coll immutable tree set
 */
object TreeSet extends MutableSortedSetFactory[TreeSet] {
  /** The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]()(ordering)
}

/**
 *  @author Lucien Pereira
 */
class TreeSet[A](base: Option[TreeSet[A]] = None, from: Option[A] = None, until: Option[A] = None)(implicit val ordering: Ordering[A]) extends SortedSet[A]
  with SetLike[A, TreeSet[A]]
  with SortedSetLike[A, TreeSet[A]]
  with Set[A] {

  private var avl: AVLTree[A] = Leaf

  private var cardinality: Int = 0

  def resolve(): TreeSet[A] = base.getOrElse(this)

  private def isLeftAcceptable(from: Option[A], ordering: Ordering[A])(a: A): Boolean =
    from.map(x => ordering.gteq(a, x)).getOrElse(true)

  private def isRightAcceptable(until: Option[A], ordering: Ordering[A])(a: A): Boolean =
    until.map(x => ordering.lt(a, x)).getOrElse(true)

  // cardinality store the set size, unfortunately a
  // set view (given by rangeImpl)
  // cannot take advantage of this optimisation
  override def size: Int = base.map(_ => super.size).getOrElse(cardinality)

  override def stringPrefix = "TreeSet"

  override def empty: TreeSet[A] = TreeSet.empty

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSet(Some(this), from, until)

  override def -=(elem: A): this.type = {
    try {
      resolve.avl = AVLTree.remove(elem, resolve.avl, ordering)
      resolve.cardinality = resolve.cardinality - 1
    } catch {
      case e: NoSuchElementException => ()
      case a: Any => a.printStackTrace
    }
    assert(2 > math.abs(resolve.avl.balance))
    this
  }

  override def +=(elem: A): this.type = {
    try {
      resolve.avl = AVLTree.insert(elem, resolve.avl, ordering)
      resolve.cardinality = resolve.cardinality + 1
    } catch {
      case e: IllegalArgumentException => ()
      case a: Any => a.printStackTrace
    }
    assert(2 > math.abs(resolve.avl.balance))
    this
  }

  override def +(elem: A): TreeSet[A] = {
    val t = TreeSet[A]()
    try {
      t.avl = AVLTree.insert(elem, resolve.avl, ordering)
      t.cardinality = resolve.cardinality + 1
    } catch {
      case e: IllegalArgumentException => {
        t.avl = resolve.avl
        t.cardinality = resolve.cardinality
      }
      case a: Any => a.printStackTrace
    }
    assert(2 > math.abs(resolve.avl.balance))
    t
  }

  override def -(elem: A): TreeSet[A] = {
    val t = TreeSet[A]()
    try {
      t.avl = AVLTree.remove(elem, resolve.avl, ordering)
      t.cardinality = resolve.cardinality - 1
    } catch {
      case e: NoSuchElementException => {
        t.avl = resolve.avl
        t.cardinality = resolve.cardinality
      }
      case a: Any => a.printStackTrace
    }
    assert(2 > math.abs(resolve.avl.balance))
    t
  }

  override def clone: TreeSet[A] = empty ++= this

  override def contains(elem: A): Boolean = AVLTree.contains(elem, resolve.avl, ordering)

  override def iterator: Iterator[A] = AVLTree.iterator(resolve.avl, isLeftAcceptable(from, ordering), isRightAcceptable(until, ordering))

}
